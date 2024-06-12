const std = @import("std");
const builtin = @import("builtin");

// const Pool: fn (comptime type) type = if (builtin.single_threaded) {
//     @import("../fakepool.zig").Pool;
// } else {
//     @compileError("multi-threaded not supported yet");
// };

pub fn HashMap(comptime K: type, comptime V: type, comptime Context: type) type {
    return struct {
        size: usize,
        root: PoolRef(Node(struct { K, V })),
    };
}
