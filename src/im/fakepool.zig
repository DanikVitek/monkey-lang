pub fn Pool(comptime T: type) type {
    _ = T;
    return struct {
        const Self = @This();

        pub fn init(size: usize) Self {
            _ = size;
            return Self{};
        }

        pub fn get_pool_size(self: *const Self) usize {
            _ = self;
            return 0;
        }

        pub fn fill(self: *const Self) void {
            _ = self;
        }

        pub fn clone(self: *const Self) Self {
            return self.*;
        }
    };
}
