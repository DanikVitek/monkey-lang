const std = @import("std");
const builtin = @import("builtin");

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
        pub const key_eql = getAutoEqlFn(K, @This());
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
        const t: Log2IntCeil(Log2Int(Digest)) = @intCast(@typeInfo(Log2Int(Digest)).Int.bits);
        const max_depth: usize = std.math.divCeil(usize, table_size, t) catch unreachable;

        root: *const Branch,
        allocator: Allocator,

        const Node = struct {
            ref_count: RefCount = if (builtin.single_threaded) 1 else AtomicUsize.init(1),
            impl: NodeImpl,

            const RefCount = if (builtin.single_threaded)
                usize
            else
                AtomicUsize;

            inline fn addRef(self: *const Node) void {
                const ref_count: *RefCount = @constCast(&self.ref_count);

                if (builtin.single_threaded)
                    ref_count.* += 1
                else
                    _ = ref_count.fetchAdd(1, .release);
            }

            inline fn deinit(self: *const Node, allocator: Allocator) void {
                const cleanup = struct {
                    fn cleanup(_: void, _: K, _: V) void {}
                }.cleanup;
                self.deinitCleanup(allocator, {}, cleanup);
            }

            fn deinitCleanup(
                self: *const Node,
                allocator: Allocator,
                cleanup_ctx: anytype,
                comptime cleanupFn: fn (ctx: @TypeOf(cleanup_ctx), key: K, value: V) void,
            ) void {
                const ref_count: *RefCount = @constCast(&self.ref_count);

                if (builtin.single_threaded) {
                    const prev_ref_count: usize = ref_count.*;
                    ref_count.* = prev_ref_count - 1;
                    if (prev_ref_count == 1) {
                        switch (self.impl) {
                            .branch => |branch| branch.deinitCleanup(allocator, cleanup_ctx, cleanupFn),
                            .leaf => |leaf| leaf.deinitCleanup(allocator, cleanup_ctx, cleanupFn),
                        }
                        allocator.destroy(self);
                    }
                } else if (ref_count.fetchSub(1, .release) == 1) {
                    ref_count.fence(.acquire);
                    switch (self.impl) {
                        .branch => |branch| branch.deinitCleanup(allocator, cleanup_ctx, cleanupFn),
                        .leaf => |leaf| leaf.deinitCleanup(allocator, cleanup_ctx, cleanupFn),
                    }
                    allocator.destroy(self);
                }
            }

            inline fn cast(self: *const Node, comptime T: type) *const T {
                self.validateCast(T);
                return switch (self.impl) {
                    .branch => |*branch| if (T == Leaf) unreachable else branch,
                    .leaf => |*leaf| if (T == Branch) unreachable else leaf,
                };
            }

            inline fn castMut(self: *Node, comptime T: type) *T {
                self.validateCast(T);
                return switch (self.impl) {
                    .branch => |*branch| if (T == Leaf) unreachable else branch,
                    .leaf => |*leaf| if (T == Branch) unreachable else leaf,
                };
            }

            inline fn validateCast(self: *const Node, comptime T: type) void {
                if (T == Branch)
                    std.debug.assert(self.impl == .branch)
                else if (T == Leaf)
                    std.debug.assert(self.impl == .leaf)
                else
                    @compileError("T can only be Branch or Leaf");
            }
        };

        const NodeImpl = union(enum) {
            branch: Branch,
            leaf: Leaf,
        };

        const Branch = struct {
            bitmap: Digest = 0,
            len: Log2IntCeil(Digest) = 0,
            base: [*]*const Node = undefined,

            fn node(self: Branch) Node {
                return Node{ .impl = .{ .branch = self } };
            }

            fn asNode(self: *const Branch) *const Node {
                return @fieldParentPtr("impl", @as(*const NodeImpl, @fieldParentPtr("branch", self)));
            }

            fn asNodeMut(self: *Branch) *Node {
                return @fieldParentPtr("impl", @as(*NodeImpl, @fieldParentPtr("branch", self)));
            }

            fn initCapacity(allocator: Allocator, size: Log2IntCeil(Digest), bitmap: Digest) Allocator.Error!Branch {
                // std.debug.print(std.fmt.comptimePrint("size: {{d}}; bitmap: {{b:0>{d}}}\n", .{@typeInfo(Digest).Int.bits}), .{ size, bitmap });
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

            inline fn deinit(self: Branch, allocator: Allocator) void {
                const cleanup = struct {
                    fn cleanup(_: void, _: K, _: V) void {}
                }.cleanup;
                self.deinitCleanup(allocator, undefined, cleanup);
            }

            fn deinitCleanup(
                self: Branch,
                allocator: Allocator,
                cleanup_ctx: anytype,
                comptime cleanupFn: fn (ctx: @TypeOf(cleanup_ctx), key: K, value: V) void,
            ) void {
                std.debug.assert(self.len == @popCount(self.bitmap));
                const slice = self.base[0..self.len];
                for (slice) |stored_node| {
                    stored_node.deinitCleanup(allocator, cleanup_ctx, cleanupFn);
                }
                allocator.free(slice);
            }

            fn withInserted(
                self: *const Branch,
                allocator: Allocator,
                idx: SparseIndex,
                child: *const Node,
            ) Allocator.Error!Branch {
                std.debug.assert(!idx.bitIsSet(self.bitmap));
                std.debug.assert(self.len == @popCount(self.bitmap));

                const new_bitmap = idx.bitPosition() | self.bitmap;

                const old_size = self.len;

                const new_table = try initCapacity(allocator, old_size + 1, new_bitmap);

                const split_point = idx.toCompactIndex(self.bitmap)[0];

                for (0..split_point) |i| {
                    const sharedNode = self.base[i];
                    new_table.base[i] = sharedNode;
                    sharedNode.addRef();
                }

                new_table.base[split_point] = child;

                for (split_point..old_size) |i| {
                    const sharedNode = self.base[i];
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
                std.debug.assert(self.len == @popCount(self.bitmap));

                const new_bitmap = idx.bitPosition() | self.bitmap;

                const old_size = self.len;

                const new_table = try initCapacity(allocator, old_size, new_bitmap);

                const split_point = idx.toCompactIndex(self.bitmap)[0];

                for (0..split_point) |i| {
                    const sharedNode = self.base[i];
                    new_table.base[i] = sharedNode;
                    sharedNode.addRef();
                }

                new_table.base[split_point] = child;

                for (split_point + 1..old_size) |i| {
                    const sharedNode = self.base[i];
                    new_table.base[i] = sharedNode;
                    sharedNode.addRef();
                }

                return new_table;
            }

            fn clone(self: *const Branch, allocator: Allocator) Allocator.Error!Branch {
                const new_table = try initCapacity(allocator, self.len, self.bitmap);
                for (0..self.len) |i| {
                    const sharedNode = self.base[i];
                    new_table.base[i] = sharedNode;
                    sharedNode.addRef();
                }
                return new_table;
            }

            fn getAtSparse(self: *const Branch, idx: SparseIndex) ?*const Node {
                return if (idx.bitIsSet(self.bitmap))
                    self.getAtCompact(idx.toCompactIndex(self.bitmap))
                else
                    null;
            }

            fn getAtCompact(self: *const Branch, idx: CompactIndex) ?*const Node {
                const i = idx[0];
                std.debug.assert(i < self.len);
                return self.base[i];
            }
        };
        const Leaf = struct {
            hash: Digest,
            len: usize,
            ptr: *Entry,

            fn node(self: Leaf) Node {
                return Node{ .impl = .{ .leaf = self } };
            }

            fn asNode(self: *const Leaf) *const Node {
                return @fieldParentPtr("impl", @as(*const NodeImpl, @fieldParentPtr("leaf", self)));
            }

            fn asNodeMut(self: *Leaf) *Node {
                return @fieldParentPtr("impl", @as(*NodeImpl, @fieldParentPtr("leaf", self)));
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
            inline fn deinit(self: Leaf, allocator: Allocator) void {
                const cleanup = struct {
                    fn cleanup(_: void, _: K, _: V) void {}
                }.cleanup;
                self.deinitCleanup(allocator, undefined, cleanup);
            }

            /// Clears the memory of the entries
            ///
            /// Caller must deinit the memory, _refereed_ in entries, manually
            fn deinitCleanup(
                self: Leaf,
                allocator: Allocator,
                cleanup_ctx: anytype,
                comptime cleanupFn: fn (ctx: @TypeOf(cleanup_ctx), key: K, value: V) void,
            ) void {
                for (self.slice()) |entry| {
                    cleanupFn(cleanup_ctx, entry.key, entry.value);
                }
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

            pub inline fn toCompactIndex(self: SparseIndex, bitmap: Digest) CompactIndex {
                // bit = 0b0010_0000 => mask = bit - 1 = 0b0001_1111 => popCount(mask) = 5 => popCount(bitmap & mask) <= 5
                return .{@intCast(@popCount(bitmap & (self.bitPosition() - 1)))};
            }
        };

        const CompactIndex = struct { Log2Int(Digest) };

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

                var next_node = current_branch.getAtSparse(.{ .value = chunk });
                while (next_node != null and next_node.?.impl == .branch) {
                    branches[size] = current_branch;
                    hash_chunks[size] = chunk;

                    current_branch = next_node.?.cast(Branch);
                    size += 1;
                    chunked_hash.progress();

                    chunk = chunked_hash.chunk();
                    next_node = current_branch.getAtSparse(.{ .value = chunk });
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
            fn addEntryAtLeaf(
                self: *const Path,
                allocator: Allocator,
                key: K,
                value: V,
                ctx: Context,
                val_eql_ctx: anytype,
                comptime valEql: fn (@TypeOf(val_eql_ctx), a: V, b: V) bool,
            ) Allocator.Error!?*const Branch {
                std.debug.assert(self.leaf != null);

                const existing_leaf = self.leaf.?;
                if (existing_leaf.findValue(key, ctx)) |stored_value| {
                    if (valEql(val_eql_ctx, stored_value.*, value)) return null; // if the same value already exists, do nothing
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

        pub inline fn deinit(self: Self) void {
            self.root.asNode().deinit(self.allocator);
        }

        pub inline fn deinitCleanup(
            self: Self,
            cleanup_ctx: anytype,
            comptime cleanupFn: fn (ctx: @TypeOf(cleanup_ctx), key: K, value: V) void,
        ) void {
            self.root.asNode().deinitCleanup(self.allocator, cleanup_ctx, cleanupFn);
        }

        pub fn clone(self: *const Self) Allocator.Error!Self {
            const new_root = try self.root.clone(self.allocator);
            const new_root_node = try self.allocator.create(Node);
            new_root_node.* = new_root.node();
            return .{
                .root = new_root_node.cast(Branch),
                .allocator = self.allocator,
            };
        }

        pub inline fn get(self: *const Self, key: K) ?V {
            return self.getContext(key, undefined);
        }

        pub inline fn getContext(self: *const Self, key: K, ctx: Context) ?V {
            const maybe_entry = self.getEntryPtrContext(key, ctx) orelse return null;
            return maybe_entry.value;
        }

        pub inline fn getPtr(self: *const Self, key: K) ?*V {
            return self.getPtrContext(key, undefined);
        }

        pub inline fn getPtrContext(self: *const Self, key: K, ctx: Context) ?*V {
            const maybe_entry = self.getEntryPtrContext(key, ctx) orelse return null;
            return &maybe_entry.value;
        }

        pub inline fn getEntryPtr(self: *const Self, key: K) ?*Entry {
            return self.getEntryPtrContext(key, undefined);
        }

        pub fn getEntryPtrContext(self: *const Self, key: K, ctx: Context) ?*Entry {
            var chunked_hash: ChunkedHash = .{ .hash = ctx.hash(key) };

            var sparse_idx: SparseIndex = .{ .value = chunked_hash.chunk() };
            if (!sparse_idx.bitIsSet(self.root.bitmap)) return null; // hash table entry is empty

            var current: *const Node = self.root.base[sparse_idx.toCompactIndex(self.root.bitmap)[0]];

            chunked_hash.progress();

            while (true) switch (current.impl) {
                .branch => |*branch| {
                    sparse_idx.value = chunked_hash.chunk();
                    if (!sparse_idx.bitIsSet(branch.bitmap)) return null;

                    current = branch.base[sparse_idx.toCompactIndex(branch.bitmap)[0]];

                    chunked_hash.progress();
                },
                .leaf => |*leaf| return leaf.find(key, ctx),
            };
        }

        pub inline fn insert(
            self: *Self,
            key: K,
            value: V,
            comptime valEql: fn (a: V, b: V) bool,
        ) Allocator.Error!void {
            return self.insertContext(key, value, undefined, {}, struct {
                fn valEqlCtx(_: void, a: V, b: V) bool {
                    return valEql(a, b);
                }
            }.valEqlCtx);
        }

        pub fn insertContext(
            self: *Self,
            key: K,
            value: V,
            ctx: Context,
            val_eql_ctx: anytype,
            comptime valEql: fn (@TypeOf(val_eql_ctx), a: V, b: V) bool,
        ) Allocator.Error!void {
            const new_root = try self.insertedBranch(key, value, ctx, val_eql_ctx, valEql) orelse return;
            self.deinit();
            self.root = new_root;
        }

        pub inline fn inserted(
            self: Self,
            key: K,
            value: V,
            comptime valEql: fn (a: V, b: V) bool,
        ) Allocator.Error!Self {
            return self.insertedContext(key, value, undefined, {}, struct {
                fn valEqlCtx(_: void, a: V, b: V) bool {
                    return valEql(a, b);
                }
            }.valEqlCtx);
        }

        pub fn insertedContext(
            self: Self,
            key: K,
            value: V,
            ctx: Context,
            val_eql_ctx: anytype,
            comptime valEql: fn (@TypeOf(val_eql_ctx), a: V, b: V) bool,
        ) Allocator.Error!Self {
            return .{
                .root = (try self.insertedBranch(key, value, ctx, val_eql_ctx, valEql)) orelse return self,
                .allocator = self.allocator,
            };
        }

        fn insertedBranch(
            self: *const Self,
            key: K,
            value: V,
            ctx: Context,
            val_eql_ctx: anytype,
            comptime valEql: fn (@TypeOf(val_eql_ctx), a: V, b: V) bool,
        ) Allocator.Error!?*const Branch {
            const path = Path.init(key, self.root, ctx);
            return if (path.leaf != null)
                try path.addEntryAtLeaf(self.allocator, key, value, ctx, val_eql_ctx, valEql)
            else
                try path.addEntryAtUnsetPosition(self.allocator, Leaf{ .hash = path.chunked_hash.hash, .len = 1, .ptr = b: {
                    const ptr = try self.allocator.create(Entry);
                    ptr.* = .{ .key = key, .value = value };
                    break :b ptr;
                } });
        }

        pub const Iterator = struct {
            levels: std.BoundedArray(Level, max_depth) = .{},
            leaf: ?LeafEntry = null,

            const Level = struct {
                branch: *const Branch,
                idx: Log2Int(Digest) = 0,
            };
            const LeafEntry = struct {
                leaf: *const Leaf,
                idx: usize = 0,
            };

            pub fn init(root: *const Branch) Iterator {
                var iter = Iterator{};
                if (root.len == 0) return iter;
                iter.descendFrom(root, 0);
                return iter;
            }

            pub fn next(self: *Iterator) ?*Entry {
                if (self.leaf == null) return null;

                const entry: *Entry = &self.leaf.?.leaf.sliceMut()[self.leaf.?.idx];

                if (self.leaf.?.idx + 1 < self.leaf.?.leaf.len) {
                    self.leaf.?.idx += 1;
                    return entry;
                }

                self.leaf = null;

                var level: *Level = &self.levels.buffer[self.levels.len - 1];

                while (level.idx + 1 == level.branch.len) {
                    self.levels.len -= 1;
                    if (self.levels.len == 0) return entry;
                    level = &self.levels.buffer[self.levels.len - 1];
                }

                level.idx += 1;

                const next_node: *const Node = level.branch.getAtCompact(.{level.idx}).?;
                switch (next_node.impl) {
                    .leaf => |*next_leaf| {
                        self.leaf = .{ .leaf = next_leaf };
                    },
                    .branch => |*next_branch| self.descendFrom(next_branch, self.levels.len - 1),
                }

                return entry;
            }

            /// root must not be empty
            fn descendFrom(self: *Iterator, root: *const Branch, start_depth: IntFittingRange(0, max_depth)) void {
                var leaf: ?*const Leaf = null;
                var depth = start_depth;
                var branch = root;
                while (leaf == null) {
                    std.debug.assert(branch.len > 0);

                    self.levels.buffer[depth] = .{ .branch = branch };
                    depth += 1;

                    const next_node = branch.getAtCompact(.{0}).?;
                    switch (next_node.impl) {
                        .leaf => |*next_leaf| leaf = next_leaf,
                        .branch => |*next_branch| {
                            std.debug.assert(next_branch.len > 1);
                            branch = next_branch;
                        },
                    }
                }
                std.debug.assert(leaf.?.len == 1);
                self.leaf = .{ .leaf = leaf.? };
                self.levels.len = depth;
            }
        };

        pub fn iterator(self: *const Self) Iterator {
            return Iterator.init(self.root);
        }

        test Iterator {
            const HAMT = StringHashArrayMappedTrie(i32);
            const testing = std.testing;

            var trie = try HAMT.init(testing.allocator);
            defer trie.deinit();

            const valEql = struct {
                fn valEql(a: i32, b: i32) bool {
                    return a == b;
                }
            }.valEql;

            try trie.insert("foo", 42, valEql);
            try trie.insert("bar", 43, valEql);
            try trie.insert("baz", 44, valEql);

            var iter = trie.iterator();

            try testing.expectEqual(@as(IntFittingRange(0, max_depth), 1), iter.levels.len);
            try testing.expectEqual(@as(Log2IntCeil(Digest), 3), iter.levels.buffer[0].branch.len);
            try testing.expectEqual(@as(Log2Int(Digest), 0), iter.levels.buffer[0].idx);

            var sum: i32 = 0;
            while (iter.next()) |entry| {
                sum += entry.value;
            }

            try testing.expectEqual(42 + 43 + 44, sum);
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
    const testing = std.testing;

    var trie = try StringTrie.init(testing.allocator);
    defer trie.deinit();

    try testing.expectEqualDeep(@as(?*const Pair, null), trie.getEntryPtr("sdvx"));

    const valEql = struct {
        fn valEql(_: void, _: void) bool {
            return true;
        }
    }.valEql;

    try trie.insert("sdvx", {}, valEql);

    try testing.expectEqualDeep(@as(?*const Pair, &.{ .key = "sdvx", .value = {} }), trie.getEntryPtr("sdvx"));
    try testing.expectEqualDeep(@as(?*const Pair, null), trie.getEntryPtr(""));

    try trie.insert("", {}, valEql);
    try testing.expectEqualDeep(@as(?*const Pair, &.{ .key = "", .value = {} }), trie.getEntryPtr(""));
}

test "trie get immutably inserted entry" {
    const Pair = StringTrie.Entry;
    const testing = std.testing;

    const t1 = try StringTrie.init(testing.allocator);
    defer t1.deinit();

    try testing.expectEqualDeep(@as(?*const Pair, null), t1.getEntryPtr("sdvx"));

    const valEql = struct {
        fn valEql(_: void, _: void) bool {
            return true;
        }
    }.valEql;

    const t2 = try t1.inserted("sdvx", {}, valEql);
    defer t2.deinit();

    try testing.expectEqualDeep(@as(?*const Pair, null), t1.getEntryPtr("sdvx"));
    try testing.expectEqualDeep(@as(?*const Pair, &.{ .key = "sdvx", .value = {} }), t2.getEntryPtr("sdvx"));
    try testing.expectEqualDeep(@as(?*const Pair, null), t2.getEntryPtr(""));

    const t3 = try t2.inserted("", {}, valEql);
    defer t3.deinit();

    try testing.expectEqualDeep(@as(?*const Pair, null), t1.getEntryPtr("sdvx"));
    try testing.expectEqualDeep(@as(?*const Pair, &.{ .key = "sdvx", .value = {} }), t2.getEntryPtr("sdvx"));
    try testing.expectEqualDeep(@as(?*const Pair, null), t2.getEntryPtr(""));
    try testing.expectEqualDeep(@as(?*const Pair, &.{ .key = "", .value = {} }), t3.getEntryPtr(""));
}

test "trie get inplace-inserted entry for the same key" {
    const HAMT = StringHashArrayMappedTrie(u32);
    const Pair = HAMT.Entry;
    const testing = std.testing;

    var trie = try HAMT.init(testing.allocator);
    defer trie.deinit();

    try testing.expectEqualDeep(@as(?*const Pair, null), trie.getEntryPtr("sdvx"));

    const valEql = struct {
        fn valEql(a: u32, b: u32) bool {
            return a == b;
        }
    }.valEql;

    try trie.insert("sdvx", 1, valEql);
    try testing.expectEqualDeep(@as(?*const Pair, &.{ .key = "sdvx", .value = 1 }), trie.getEntryPtr("sdvx"));

    try trie.insert("sdvx", 2, valEql);
    try testing.expectEqualDeep(@as(?*const Pair, &.{ .key = "sdvx", .value = 2 }), trie.getEntryPtr("sdvx"));
}

test "trie get immutably inserted entry for the same key" {
    const HAMT = StringHashArrayMappedTrie(u32);
    const Pair = HAMT.Entry;
    const testing = std.testing;

    const t1 = try HAMT.init(testing.allocator);
    defer t1.deinit();

    try testing.expectEqualDeep(@as(?*const Pair, null), t1.getEntryPtr("sdvx"));

    const valEql = struct {
        fn valEql(a: u32, b: u32) bool {
            return a == b;
        }
    }.valEql;

    const t2 = try t1.inserted("sdvx", 1, valEql);
    defer t2.deinit();

    try testing.expectEqualDeep(@as(?*const Pair, null), t1.getEntryPtr("sdvx"));
    try testing.expectEqualDeep(@as(?*const Pair, &.{ .key = "sdvx", .value = 1 }), t2.getEntryPtr("sdvx"));

    const t3 = try t2.inserted("sdvx", 2, valEql);
    defer t3.deinit();

    try testing.expectEqualDeep(@as(?*const Pair, null), t1.getEntryPtr("sdvx"));
    try testing.expectEqualDeep(@as(?*const Pair, &.{ .key = "sdvx", .value = 1 }), t2.getEntryPtr("sdvx"));
    try testing.expectEqualDeep(@as(?*const Pair, &.{ .key = "sdvx", .value = 2 }), t3.getEntryPtr("sdvx"));
}
