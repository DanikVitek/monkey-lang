const std = @import("std");

const Allocator = std.mem.Allocator;
const Log2Int = std.math.Log2Int;
const IntFittingRange = std.math.IntFittingRange;
const Log2IntCeil = std.math.Log2IntCeil;
const log2_int_ceil = std.math.log2_int_ceil;

const AtomicUsize = std.atomic.Value(usize);

const Cell = @import("../cell.zig").Cell;

pub const StringTrie = StringHashArrayMappedTrie(void);

pub fn StringHashArrayMappedTrie(comptime V: type) type {
    return HashArrayMappedTrie([]const u8, V, StringContext);
}

pub const StringContext = std.hash_map.StringContext;

pub fn AutoHashArrayMappedTrie(comptime K: type, comptime V: type) type {
    return HashArrayMappedTrie(K, V, AutoContext(K));
}

pub fn AutoContext(comptime K: type) type {
    return struct {
        pub const hash = getAutoHashFn(K, @This());
        pub const eql = getAutoEqlFn(K, @This());
    };
}

pub fn getAutoHashFn(comptime K: type, comptime Context: type) (fn (Context, K) u64) {
    comptime {
        std.debug.assert(@hasDecl(@This(), "StringHashArrayMappedTrie")); // detect when the following message needs updated
        if (K == []const u8) {
            @compileError("std.auto_hash.autoHash does not allow slices here (" ++
                @typeName(K) ++
                ") because the intent is unclear. " ++
                "Consider using std.StringHashMap for hashing the contents of []const u8. " ++
                "Alternatively, consider using std.auto_hash.hash or providing your own hash function instead.");
        }
    }

    return struct {
        fn hash(ctx: Context, key: K) u64 {
            _ = ctx;
            if (std.meta.hasUniqueRepresentation(K)) {
                return std.hash.Wyhash.hash(0, std.mem.asBytes(&key));
            } else {
                var hasher = std.hash.Wyhash.init(0);
                std.hash.autoHash(&hasher, key);
                return hasher.final();
            }
        }
    }.hash;
}

pub fn getAutoEqlFn(comptime K: type, comptime Context: type) (fn (Context, K, K) bool) {
    return struct {
        fn eql(ctx: Context, a: K, b: K) bool {
            _ = ctx;
            return std.meta.eql(a, b);
        }
    }.eql;
}

/// https://idea.popcount.org/2012-07-25-introduction-to-hamt/
pub fn HashArrayMappedTrie(
    comptime K: type,
    comptime V: type,
    comptime Context: type,
) type {
    verify(K, Context);

    return struct {
        const Self = @This();

        const Digest = @typeInfo(@TypeOf(switch (@typeInfo(Context)) {
            .Pointer => |Pointer| Pointer.child,
            else => Context,
        }.hash)).Fn.return_type.?; // as in Hash Code or Hash Digest

        const hash_size: Log2IntCeil(Digest) = @intCast(@typeInfo(Digest).Int.bits);
        const table_size: Log2IntCeil(Digest) = hash_size;
        const t: Log2IntCeil(Log2IntCeil(Digest)) = @intCast(@typeInfo(Log2IntCeil(Digest)).Int.bits);
        const max_depth: usize = std.math.divCeil(usize, table_size, t) catch unreachable;

        root: *const Branch,
        allocator: Allocator,

        const Node = struct {
            ref_count: AtomicUsize = AtomicUsize.init(1),
            kind: NodeKind,
            impl: NodeImpl,

            inline fn addRef(self: *const Node) void {
                const ref_count: *AtomicUsize = @constCast(&self.ref_count);
                _ = ref_count.fetchAdd(1, .release);
            }

            fn deinit(self: *const Node, allocator: Allocator) void {
                const ref_count: *AtomicUsize = @constCast(&self.ref_count);
                if (ref_count.fetchSub(1, .release) == 1) {
                    @fence(.acquire);
                    switch (self.kind) {
                        .branch => self.impl.branch.deinit(allocator),
                        .leaf => self.impl.leaf.deinit(allocator),
                    }
                    allocator.destroy(self);
                }
            }

            inline fn cast(self: *const Node, comptime T: type) *const T {
                self.validateCast(T);
                return switch (self.kind) {
                    .branch => if (T == Leaf) unreachable else &self.impl.branch,
                    .leaf => if (T == Branch) unreachable else &self.impl.leaf,
                };
            }

            inline fn castMut(self: *Node, comptime T: type) *T {
                self.validateCast(T);
                return switch (self.kind) {
                    .branch => if (T == Leaf) unreachable else &self.impl.branch,
                    .leaf => if (T == Branch) unreachable else &self.impl.leaf,
                };
            }

            inline fn validateCast(self: *const Node, comptime T: type) void {
                if (T == Branch)
                    std.debug.assert(self.kind == .branch)
                else if (T == Leaf)
                    std.debug.assert(self.kind == .leaf)
                else
                    @compileError("T can only be Branch or Leaf");
            }
        };

        const NodeKind = enum {
            branch,
            leaf,
        };

        const NodeImpl = extern union {
            branch: Branch,
            leaf: Leaf,
        };

        const Branch = extern struct {
            bitmap: Digest align(@alignOf(usize)) = 0,
            len: usize = 0,
            base: [*]*const Node = undefined,

            fn node(self: Branch) Node {
                return Node{ .impl = .{ .branch = self }, .kind = .branch };
            }

            fn asNode(self: *const Branch) *const Node {
                return @fieldParentPtr("impl", @as(*const NodeImpl, @ptrCast(@alignCast(self))));
            }

            fn asNodeMut(self: *Branch) *Node {
                return @fieldParentPtr("impl", @as(*NodeImpl, @ptrCast(@alignCast(self))));
            }

            fn initCapacity(allocator: Allocator, size: Log2IntCeil(Digest), bitmap: Digest) Allocator.Error!Branch {
                std.debug.print(std.fmt.comptimePrint("size: {{d}}; bitmap: {{b:0>{d}}}\n", .{@typeInfo(Digest).Int.bits}), .{ size, bitmap });
                std.debug.assert(size <= @typeInfo(Digest).Int.bits);
                std.debug.assert(size == @popCount(bitmap));
                const mem = try allocator.alloc(*const Node, size);
                return Branch{
                    .bitmap = bitmap,
                    .len = size,
                    .base = mem.ptr,
                };
            }

            fn initSingle(allocator: Allocator, idx: SparseIndex, child: *const Node) Allocator.Error!Branch {
                var table = try initCapacity(allocator, 1, idx.bitPosition());
                table.base[0] = child;
                return table;
            }

            /// `Leaf` pointers must come from `Node`s
            fn initPair(
                allocator: Allocator,
                idx1: SparseIndex,
                leaf1: *const Leaf,
                idx2: SparseIndex,
                leaf2: *const Leaf,
            ) Allocator.Error!Branch {
                var table = try initCapacity(allocator, 2, idx1.bitPosition() | idx2.bitPosition());
                if (idx1.value < idx2.value) {
                    table.base[0] = leaf1.asNode();
                    table.base[1] = leaf2.asNode();
                } else {
                    table.base[0] = leaf2.asNode();
                    table.base[1] = leaf1.asNode();
                }
                return table;
            }

            fn deinit(self: Branch, allocator: Allocator) void {
                std.debug.assert(self.len == @popCount(self.bitmap));
                for (0..@popCount(self.bitmap)) |i| {
                    self.base[i].deinit(allocator);
                }
                allocator.free(self.base[0..self.len]);
            }

            fn withInserted(
                self: *const Branch,
                allocator: Allocator,
                idx: SparseIndex,
                child: *const Node,
            ) Allocator.Error!Branch {
                std.debug.assert(!idx.bitIsSet(self.bitmap));

                const new_bitmap = idx.bitPosition() | self.bitmap;

                const old_size = @popCount(self.bitmap);

                const new_table = try initCapacity(allocator, old_size + 1, new_bitmap);

                const split_point = idx.toCompactIndex(self.bitmap);

                for (0..split_point) |i| {
                    var sharedNode = self.base[i];
                    new_table.base[i] = sharedNode;
                    sharedNode.addRef();
                }

                new_table.base[split_point] = child;

                for (split_point..old_size) |i| {
                    var sharedNode = self.base[i];
                    new_table.base[i + 1] = sharedNode;
                    sharedNode.addRef();
                }

                return new_table;
            }

            fn withReplaced(
                self: *const Branch,
                allocator: Allocator,
                idx: SparseIndex,
                child: *const Node,
            ) Allocator.Error!Branch {
                std.debug.assert(idx.bitIsSet(self.bitmap));

                const new_bitmap = idx.bitPosition() | self.bitmap;

                const old_size = @popCount(self.bitmap);

                const new_table = try initCapacity(allocator, old_size, new_bitmap);

                const split_point = idx.toCompactIndex(self.bitmap);

                for (0..split_point) |i| {
                    var sharedNode = self.base[i];
                    new_table.base[i] = sharedNode;
                    sharedNode.addRef();
                }

                new_table.base[split_point] = child;

                for (split_point + 1..old_size) |i| {
                    var sharedNode = self.base[i];
                    new_table.base[i] = sharedNode;
                    sharedNode.addRef();
                }

                return new_table;
            }

            fn getAt(self: *const Branch, idx: SparseIndex) ?*const Node {
                return if (idx.bitIsSet(self.bitmap))
                    self.base[idx.toCompactIndex(self.bitmap)]
                else
                    null;
            }
        };
        const Leaf = extern struct {
            hash: Digest align(@alignOf(usize)),
            len: usize,
            ptr: *Entry,

            fn node(self: Leaf) Node {
                return Node{ .impl = .{ .leaf = self }, .kind = .leaf };
            }

            fn asNode(self: *const Leaf) *const Node {
                return @fieldParentPtr("impl", @as(*const NodeImpl, @ptrCast(@alignCast(self))));
            }

            fn asNodeMut(self: *Leaf) *Node {
                return @fieldParentPtr("impl", @as(*const NodeImpl, @ptrCast(@alignCast(self))));
            }

            pub inline fn fromSlice(hash: Digest, entries: []Entry) Leaf {
                std.debug.assert(entries.len > 0);
                return .{
                    .hash = hash,
                    .len = entries.len,
                    .ptr = @ptrCast(entries.ptr),
                };
            }

            pub inline fn slice(self: Leaf) []const Entry {
                std.debug.assert(self.len > 0);
                return @as([*]const Entry, @ptrCast(self.ptr))[0..self.len];
            }

            pub inline fn sliceMut(self: Leaf) []Entry {
                std.debug.assert(self.len > 0);
                return @as([*]Entry, @ptrCast(self.ptr))[0..self.len];
            }

            pub fn find(self: Leaf, key: K, ctx: Context) ?*Entry {
                for (self.sliceMut()) |*entry| {
                    if (ctx.eql(entry.key, key)) return entry;
                }
                return null;
            }

            pub fn findValue(self: Leaf, key: K, ctx: Context) ?*V {
                for (self.sliceMut()) |*entry| {
                    if (ctx.eql(entry.key, key)) return &entry.value;
                }
                return null;
            }

            pub fn findIndex(self: Leaf, key: K, ctx: Context) ?usize {
                for (self.slice(), 0..) |*entry, i| {
                    if (ctx.eql(entry.key, key)) return i;
                }
                return null;
            }

            fn withAppendedValue(self: *const Leaf, allocator: Allocator, key: K, value: V) Allocator.Error!*const Leaf {
                const new_leaf = try Leaf.initUndefined(allocator, self.hash, self.len + 1);
                for (self.slice(), new_leaf.sliceMut()) |old_entry, *new_entry| {
                    new_entry.* = old_entry;
                }
                new_leaf.sliceMut()[self.len] = .{ .key = key, .value = value };
                const new_leaf_node = try allocator.create(Node);
                new_leaf_node.* = new_leaf.node();
                return new_leaf_node.cast(Leaf);
            }

            fn withReplacedValue(self: *const Leaf, allocator: Allocator, key: K, value: V, ctx: Context) Allocator.Error!*const Leaf {
                const new_leaf = try Leaf.initUndefined(allocator, self.hash, self.len);
                for (self.slice(), new_leaf.sliceMut()) |old_entry, *new_entry| {
                    new_entry.* = if (ctx.eql(old_entry.key, key))
                        .{ .key = key, .value = value }
                    else
                        old_entry;
                }
                const new_leaf_node = try allocator.create(Node);
                new_leaf_node.* = new_leaf.node();
                return new_leaf_node.cast(Leaf);
            }

            fn initUndefined(allocator: Allocator, hash: Digest, len: usize) Allocator.Error!Leaf {
                std.debug.assert(len > 0);
                const mem = try allocator.alloc(Entry, len);
                return Leaf{
                    .hash = hash,
                    .len = len,
                    .ptr = @ptrCast(mem.ptr),
                };
            }

            /// Clears the memory of the entries
            ///
            /// Caller must deinit the memory, _refereed_ in entries, manually
            pub inline fn deinit(self: Leaf, allocator: Allocator) void {
                allocator.free(self.slice());
            }
        };
        const Entry = struct {
            key: K,
            value: V,
        };

        const SparseIndex = packed struct {
            value: Log2Int(Digest),

            pub inline fn bitPosition(self: SparseIndex) Digest {
                return @as(Digest, 1) << self.value; // value = 5 => bit = 0b0010_0000
            }

            pub inline fn bitIsSet(self: SparseIndex, bitmap: Digest) bool {
                return bitmap & self.bitPosition() != 0;
            }

            pub inline fn toCompactIndex(self: SparseIndex, bitmap: Digest) Log2IntCeil(Digest) {
                // bit = 0b0010_0000 => mask = bit - 1 = 0b0001_1111 => popCount(mask) = 5 => popCount(bitmap & mask) <= 5
                return @popCount(bitmap & (self.bitPosition() - 1));
            }
        };

        const ChunkedHash = struct {
            hash: Digest,
            offset: Log2Int(Digest) = bits_ber_chunk,

            const bits_ber_chunk: Log2IntCeil(Log2Int(Digest)) = @intCast(@typeInfo(Log2Int(Digest)).Int.bits);
            const chunks_per_hash: comptime_int = @typeInfo(Digest).Int.bits / bits_ber_chunk;
            const chunk_mask: Chunk = (1 << bits_ber_chunk) - 1;

            pub const Chunk = Log2Int(Digest);
            pub const Skips = IntFittingRange(0, chunks_per_hash - 1);

            /// Index of a bit in a table's bitmap
            pub fn chunk(self: *const ChunkedHash) Chunk {
                return @as(Chunk, @truncate((self.hash >> self.offset))) & chunk_mask;
            }

            pub fn progress(self: *ChunkedHash) void {
                self.offset += bits_ber_chunk;
            }

            pub fn next(self: *const ChunkedHash) ChunkedHash {
                return .{ .hash = self.hash, .offset = self.offset + bits_ber_chunk };
            }

            pub fn skip(self: *ChunkedHash, count: Skips) void {
                std.debug.assert(count <= chunks_per_hash - self.offset / bits_ber_chunk);
                self.offset += count * bits_ber_chunk;
            }

            pub fn reset(self: *ChunkedHash) void {
                self.offset = bits_ber_chunk;
            }
        };

        pub const Path = struct {
            branches: [max_depth]*const Branch,
            hash_chunks: [max_depth]ChunkedHash.Chunk,
            last_branch: *const Branch,
            chunked_hash: ChunkedHash,
            leaf: ?*const Leaf,
            size: IntFittingRange(0, max_depth),

            fn init(key: K, root: *const Branch, ctx: Context) Path {
                var chunked_hash: ChunkedHash = .{ .hash = ctx.hash(key) };

                var size: IntFittingRange(0, max_depth) = 0;
                var current_branch = root;
                var chunk = chunked_hash.chunk();

                var branches: [max_depth]*const Branch = undefined;
                var hash_chunks: [max_depth]ChunkedHash.Chunk = undefined;

                var next_node = current_branch.getAt(.{ .value = chunk });
                while (next_node != null and next_node.?.kind == .branch) {
                    branches[size] = current_branch;
                    hash_chunks[size] = chunk;

                    current_branch = next_node.?.cast(Branch);
                    size += 1;
                    chunked_hash.progress();

                    chunk = chunked_hash.chunk();
                    next_node = current_branch.getAt(.{ .value = chunk });
                }

                std.debug.assert(size <= max_depth);

                return Path{
                    .branches = branches,
                    .hash_chunks = hash_chunks,
                    .last_branch = current_branch,
                    .chunked_hash = chunked_hash,
                    .leaf = if (next_node) |node| node.cast(Leaf) else null,
                    .size = size,
                };
            }

            fn rewrite(self: *const Path, allocator: Allocator, new_branch: *const Branch) Allocator.Error!*const Branch {
                var current_branch = new_branch;

                for (1..self.size + 1) |_i| {
                    const i = self.size - _i;
                    const idx = SparseIndex{ .value = self.hash_chunks[i] };

                    const parent_branch = try self.branches[i].withReplaced(allocator, idx, current_branch.asNode());
                    const parent_node = try allocator.create(Node);
                    parent_node.* = parent_branch.node();
                    current_branch = parent_node.cast(Branch);
                }

                return current_branch;
            }

            fn addEntryAtUnsetPosition(self: *const Path, allocator: Allocator, leaf: Leaf) Allocator.Error!*const Branch {
                const leaf_node = try allocator.create(Node);
                leaf_node.* = leaf.node();

                const new_branch = try self.last_branch.withInserted(allocator, .{ .value = self.chunked_hash.chunk() }, leaf_node);

                const new_branch_node = try allocator.create(Node);
                new_branch_node.* = new_branch.node();

                const new_root = try self.rewrite(allocator, new_branch_node.cast(Branch));
                return new_root;
            }

            /// If entry for the given key already exists with a different value, it will be replaced.
            /// Otherwise `null` is returned.
            fn addEntryAtLeaf(self: *const Path, allocator: Allocator, key: K, value: V, ctx: Context) Allocator.Error!?*const Branch {
                std.debug.assert(self.leaf != null);

                const existing_leaf = self.leaf.?;
                if (existing_leaf.findValue(key, ctx)) |stored_value| {
                    if (std.meta.eql(stored_value.*, value)) return null; // if the same value already exists, do nothing
                    // TODO: implement replacing the value

                    // replace the leaf with a leaf, containing the new value

                    const new_leaf = try existing_leaf.withReplacedValue(allocator, key, value, ctx);
                    const new_branch = try self.last_branch.withReplaced(
                        allocator,
                        .{ .value = self.chunked_hash.chunk() },
                        new_leaf.asNode(),
                    );
                    const new_branch_node = try allocator.create(Node);
                    new_branch_node.* = new_branch.node();
                    const new_root = try self.rewrite(allocator, new_branch_node.cast(Branch));
                    return new_root;
                }

                // If hash collision, add an extra value to the leaf
                if (self.chunked_hash.hash == existing_leaf.hash) {
                    const new_leaf = try existing_leaf.withAppendedValue(allocator, key, value);
                    const new_branch = try self.last_branch.withReplaced(
                        allocator,
                        .{ .value = self.chunked_hash.chunk() },
                        new_leaf.asNode(),
                    );
                    const new_branch_node = try allocator.create(Node);
                    new_branch_node.* = new_branch.node();
                    const new_root = try self.rewrite(allocator, new_branch_node.cast(Branch));
                    return new_root;
                }
                // Different hash, so add branches to the point they diverge
                else {
                    var existing_hash: ChunkedHash = .{ .hash = existing_leaf.hash };
                    existing_hash.skip(@intCast(self.size));

                    const new_child_branch = try extendPath(allocator, existing_hash.next(), existing_leaf, self.chunked_hash.next(), b: {
                        var new_leaf = try Leaf.initUndefined(allocator, self.chunked_hash.hash, 1);
                        new_leaf.sliceMut()[0] = .{ .key = key, .value = value };
                        const new_leaf_node = try allocator.create(Node);
                        new_leaf_node.* = new_leaf.node();
                        break :b new_leaf_node.cast(Leaf);
                    });

                    const new_branch = try self.last_branch.withReplaced(allocator, .{ .value = self.chunked_hash.chunk() }, new_child_branch.asNode());
                    const new_branch_node = try allocator.create(Node);
                    new_branch_node.* = new_branch.node();

                    const new_root = try self.rewrite(allocator, new_branch_node.cast(Branch));
                    return new_root;
                }
            }

            fn extendPath(
                allocator: Allocator,
                existing_hash: ChunkedHash,
                existing_leaf: *const Leaf,
                new_hash: ChunkedHash,
                new_leaf: *const Leaf,
            ) Allocator.Error!*const Branch {
                if (existing_hash.chunk() == new_hash.chunk()) {
                    const new_child_branch = try extendPath(allocator, existing_hash.next(), existing_leaf, new_hash.next(), new_leaf);

                    const new_parent_branch = try Branch.initSingle(allocator, .{ .value = new_hash.chunk() }, new_child_branch.asNode());
                    const new_parent_branch_node = try allocator.create(Node);
                    new_parent_branch_node.* = new_parent_branch.node();

                    return new_parent_branch_node.cast(Branch);
                } else {
                    const new_branch = try Branch.initPair(
                        allocator,
                        .{ .value = existing_hash.chunk() },
                        existing_leaf,
                        .{ .value = new_hash.chunk() },
                        new_leaf,
                    );

                    const new_branch_node = try allocator.create(Node);
                    new_branch_node.* = new_branch.node();

                    existing_leaf.asNode().addRef();

                    return new_branch_node.cast(Branch);
                }
            }
        };

        pub fn init(allocator: Allocator) Allocator.Error!Self {
            const node = try allocator.create(Node);
            node.* = (Branch{}).node();
            return Self{
                .root = node.castMut(Branch),
                .allocator = allocator,
            };
        }

        pub inline fn deinit(self: Self, allocator: Allocator) void {
            self.root.asNode().deinit(allocator);
        }

        pub inline fn get(self: *const Self, key: K) ?*V {
            return self.getContext(key, undefined);
        }

        pub inline fn getContext(self: *const Self, key: K, ctx: Context) ?*V {
            const maybe_entry = self.getEntryContext(key, ctx) orelse return null;
            return &maybe_entry.value;
        }

        pub inline fn getEntry(self: *const Self, key: K) ?*Entry {
            return self.getEntryContext(key, undefined);
        }

        pub fn getEntryContext(self: *const Self, key: K, ctx: Context) ?*Entry {
            var chunked_hash: ChunkedHash = .{ .hash = ctx.hash(key) };

            var sparse_idx: SparseIndex = .{ .value = chunked_hash.chunk() };
            if (!sparse_idx.bitIsSet(self.root.bitmap)) return null; // hash table entry is empty

            var current: *const Node = self.root.base[sparse_idx.toCompactIndex(self.root.bitmap)];

            chunked_hash.progress();

            while (true) switch (current.kind) {
                .branch => {
                    const branch: *const Branch = current.cast(Branch);

                    sparse_idx.value = chunked_hash.chunk();
                    if (!sparse_idx.bitIsSet(branch.bitmap)) return null;

                    current = branch.base[sparse_idx.toCompactIndex(branch.bitmap)];

                    chunked_hash.progress();
                },
                .leaf => {
                    const leaf: *const Leaf = current.cast(Leaf);
                    return leaf.find(key, ctx);
                },
            };
        }

        pub inline fn insert(self: *Self, allocator: Allocator, key: K, value: V) Allocator.Error!void {
            return self.insertContext(allocator, key, value, undefined);
        }

        pub fn insertContext(self: *Self, allocator: Allocator, key: K, value: V, ctx: Context) Allocator.Error!void {
            const new_root = try self.insertedBranch(allocator, key, value, ctx) orelse return;
            self.deinit(allocator);
            self.root = new_root;
        }

        pub inline fn inserted(self: Self, allocator: Allocator, key: K, value: V) Allocator.Error!Self {
            return self.insertedContext(allocator, key, value, undefined);
        }

        pub fn insertedContext(self: Self, allocator: Allocator, key: K, value: V, ctx: Context) Allocator.Error!Self {
            return .{
                .root = (try self.insertedBranch(allocator, key, value, ctx)) orelse return self,
                .allocator = allocator,
            };
        }

        fn insertedBranch(self: *const Self, allocator: Allocator, key: K, value: V, ctx: Context) Allocator.Error!?*const Branch {
            const path = Path.init(key, self.root, ctx);
            return if (path.leaf != null)
                try path.addEntryAtLeaf(allocator, key, value, ctx)
            else
                try path.addEntryAtUnsetPosition(allocator, Leaf{ .hash = path.chunked_hash.hash, .len = 1, .ptr = b: {
                    const ptr = try allocator.create(Entry);
                    ptr.* = .{ .key = key, .value = value };
                    break :b ptr;
                } });
        }

        pub fn print(self: *const Self) !void {
            const stdout = std.io.getStdOut().writer();
            var buffered = std.io.bufferedWriter(stdout);

            const w = buffered.writer();

            for (self.root, 0..) |maybe_node, i| {
                try w.print("{:0>2}: ", .{i});

                if (maybe_node) |node| {
                    try _print(w, node, 1);
                } else {
                    try w.print("null\n", .{});
                }
            }

            try buffered.flush();
        }

        fn _print(w: anytype, node: *const Node, depth: u16) !void {
            // @compileLog(@TypeOf(w));

            switch (node.*) {
                .kv => |pair| {
                    try w.print(".{{ .key = \"{s}\", .value = {} }}\n", .{ pair.key, pair.value });
                },
                .table => |table| {
                    try w.print(".{{ .map = 0x{X:0>8}, .ptr = {*} }}\n", .{ table.bitmap, table.base });

                    for (0..@popCount(table.bitmap)) |i| {
                        for (0..depth) |_| try w.print(" ", .{});
                        try w.print("{:0>2}: ", .{i});

                        try _print(w, &table.base[i], depth + 1);
                    }
                },
            }
        }
    };
}

fn verify(comptime K: type, comptime Context: type) void {
    const Container = switch (@typeInfo(Context)) {
        .Struct, .Union, .Enum, .Opaque => Context,
        .Pointer => |Pointer| if (Pointer.size == .One) switch (@typeInfo(Pointer.child)) {
            .Struct, .Union, .Enum, .Opaque => Pointer.child,
            else => @compileError("Context in a form of a pointer must be a pointer to a container type (Struct, Union, Enum, Opaque), but was " ++ @tagName(@typeInfo(Pointer.child))),
        } else @compileError("Context in a form of a pointer must be a pointer to a single item, but was a " ++ @tagName(Pointer.size) ++ " pointer"),
        else => @compileError(std.fmt.comptimePrint(
            "HAMT context must be (a pointer to) a container type with hash(Context, {0s}) Digest, and eql(Context, {0s}, {0s}) bool",
            .{@typeName(K)},
        )),
    };

    if (@hasDecl(Container, "hash")) {
        const hash = Container.hash;
        const HashFn = @TypeOf(hash);

        const info = @typeInfo(HashFn);

        if (info != .Fn) @compileError("Context.hash must be a function, however it was actually" ++ @typeName(HashFn));

        const func = info.Fn;
        if (func.params.len != 2) @compileError(std.fmt.comptimePrint(
            "Invalid Context.hash signature. Expected hash({s}, {s}), but was actually {s}",
            .{ @typeName(Context), @typeName(K), @typeName(HashFn) },
        ));

        if (func.params[0].type == null or func.params[0].type.? != Context) {
            const type_str = if (func.params[0].type) |Param| @typeName(Param) else "null";
            @compileError("Invalid Context.hash signature. The first parameter must be " ++ @typeName(Context) ++
                ", however it was " ++ type_str);
        }

        if (func.params[1].type == null or func.params[1].type.? != K) {
            const type_str = if (func.params[1].type) |Param| @typeName(Param) else "null";
            @compileError("Invalid Context.hash signature. The second parameter must be " ++ @typeName(K) ++
                ", however it was " ++ type_str);
        }

        if (func.return_type == null) {
            @compileError("Invalid Context.hash signature. Return type must be an unsigned integer, however it was null");
        }

        const Digest = func.return_type.?;
        const digest_info = @typeInfo(Digest);

        if (digest_info != .Int)
            @compileError("Invalid Context.hash signature. Return type must be an unsigned integer, however it was actually " ++ @typeName(Digest));
        if (digest_info.Int.signedness != .unsigned)
            @compileError("Invalid Context.hash signature. Return type must be an unsigned integer, however it was actually an " ++ @typeName(Digest));
    }

    if (@hasDecl(Container, "eql")) {
        const eql = Container.eql;
        const EqlFn = @TypeOf(eql);

        const info = @typeInfo(EqlFn);

        if (info != .Fn) @compileError("Context.eql must be a function, however it was actually" ++ @typeName(EqlFn));

        const func = info.Fn;
        if (func.params.len != 3) @compileError("Invalid Context.eql signature. Expected eql(" ++ @typeName(Context) ++ ", " ++ @typeName(K) ++ ", " ++ @typeName(K) ++ "), but was actually " ++ @typeName(EqlFn));

        if (func.params[0].type == null or func.params[0].type.? != Context) {
            const type_str = if (func.params[0].type) |Param| @typeName(Param) else "null";
            @compileError("Invalid Context.eql signature. The first parameter must be " ++ @typeName(Context) ++ ", however it was " ++ type_str);
        }

        if (func.params[1].type == null or func.params[1].type.? != K) {
            const type_str = if (func.params[1].type) |Param| @typeName(Param) else "null";
            @compileError("Invalid Context.eql signature. The second parameter must be " ++ @typeName(K) ++ ", however it was " ++ type_str);
        }

        if (func.params[2].type == null or func.params[2].type.? != K) {
            const type_str = if (func.params[2].type) |Param| @typeName(Param) else "null";
            @compileError("Invalid Context.eql signature. The third parameter must be " ++ @typeName(K) ++ ", however it was " ++ type_str);
        }

        if (func.return_type == null or func.return_type.? != bool) {
            const type_str = if (func.return_type) |Return| @typeName(Return) else "null";

            @compileError("Invalid Context.eql signature, Return type must be " ++ @typeName(bool) ++ ", however it was " ++ type_str);
        }
    }
}

test "trie get inplace-inserted entry" {
    const Pair = StringTrie.Entry;
    const allocator = std.testing.allocator;

    var trie = try StringTrie.init(allocator);
    defer trie.deinit(allocator);

    try std.testing.expectEqualDeep(@as(?*const Pair, null), trie.getEntry("sdvx"));

    try trie.insert(allocator, "sdvx", {});

    try std.testing.expectEqualDeep(@as(?*const Pair, &.{ .key = "sdvx", .value = {} }), trie.getEntry("sdvx"));
    try std.testing.expectEqualDeep(@as(?*const Pair, null), trie.getEntry(""));

    try trie.insert(allocator, "", {});
    try std.testing.expectEqualDeep(@as(?*const Pair, &.{ .key = "", .value = {} }), trie.getEntry(""));
}

test "trie get immutably inserted entry" {
    const Pair = StringTrie.Entry;
    const allocator = std.testing.allocator;

    const t1 = try StringTrie.init(allocator);
    defer t1.deinit(allocator);

    try std.testing.expectEqualDeep(@as(?*const Pair, null), t1.getEntry("sdvx"));

    const t2 = try t1.inserted(allocator, "sdvx", {});
    defer t2.deinit(allocator);

    try std.testing.expectEqualDeep(@as(?*const Pair, null), t1.getEntry("sdvx"));
    try std.testing.expectEqualDeep(@as(?*const Pair, &.{ .key = "sdvx", .value = {} }), t2.getEntry("sdvx"));
    try std.testing.expectEqualDeep(@as(?*const Pair, null), t2.getEntry(""));

    const t3 = try t2.inserted(allocator, "", {});
    defer t3.deinit(allocator);

    try std.testing.expectEqualDeep(@as(?*const Pair, null), t1.getEntry("sdvx"));
    try std.testing.expectEqualDeep(@as(?*const Pair, &.{ .key = "sdvx", .value = {} }), t2.getEntry("sdvx"));
    try std.testing.expectEqualDeep(@as(?*const Pair, null), t2.getEntry(""));
    try std.testing.expectEqualDeep(@as(?*const Pair, &.{ .key = "", .value = {} }), t3.getEntry(""));
}

test "trie get inplace-inserted entry for the same key" {
    const HAMT = StringHashArrayMappedTrie(u32);
    const Pair = HAMT.Entry;
    const allocator = std.testing.allocator;

    var trie = try HAMT.init(allocator);
    defer trie.deinit(allocator);

    try std.testing.expectEqualDeep(@as(?*const Pair, null), trie.getEntry("sdvx"));

    try trie.insert(allocator, "sdvx", 1);
    try std.testing.expectEqualDeep(@as(?*const Pair, &.{ .key = "sdvx", .value = 1 }), trie.getEntry("sdvx"));

    try trie.insert(allocator, "sdvx", 2);
    try std.testing.expectEqualDeep(@as(?*const Pair, &.{ .key = "sdvx", .value = 2 }), trie.getEntry("sdvx"));
}

test "trie get immutably inserted entry for the same key" {
    const HAMT = StringHashArrayMappedTrie(u32);
    const Pair = HAMT.Entry;
    const allocator = std.testing.allocator;

    const t1 = try HAMT.init(allocator);
    defer t1.deinit(allocator);

    try std.testing.expectEqualDeep(@as(?*const Pair, null), t1.getEntry("sdvx"));

    const t2 = try t1.inserted(allocator, "sdvx", 1);
    defer t2.deinit(allocator);

    try std.testing.expectEqualDeep(@as(?*const Pair, null), t1.getEntry("sdvx"));
    try std.testing.expectEqualDeep(@as(?*const Pair, &.{ .key = "sdvx", .value = 1 }), t2.getEntry("sdvx"));

    const t3 = try t2.inserted(allocator, "sdvx", 2);
    defer t3.deinit(allocator);

    try std.testing.expectEqualDeep(@as(?*const Pair, null), t1.getEntry("sdvx"));
    try std.testing.expectEqualDeep(@as(?*const Pair, &.{ .key = "sdvx", .value = 1 }), t2.getEntry("sdvx"));
    try std.testing.expectEqualDeep(@as(?*const Pair, &.{ .key = "sdvx", .value = 2 }), t3.getEntry("sdvx"));
}
