const std = @import("std");

pub fn implements(comptime Type: type, comptime Trait: type) bool {
    var has_default = false;
    inline for (std.meta.declarations(Trait)) |decl| {
        if (std.mem.eql(u8, decl.name, "default") and isContainer(Trait.default)) {
            has_default = true;
            continue;
        }
        if (!@hasDecl(Type, decl.name) or @TypeOf(@field(Type, decl.name)) != @TypeOf(@field(Trait, decl.name))) {
            return false;
        }
    }
    if (has_default) {
        inline for (std.meta.declarations(Trait.default)) |decl| {
            if (!@hasDecl(Type, decl.name) or @TypeOf(@field(Type, decl.name)) != @TypeOf(@field(Trait.default, decl.name))) {
                return false;
            }
        }
    }

    return true;
}

fn isContainer(comptime T: type) bool {
    switch (@typeInfo(T)) {
        .Struct, .Enum, .Union, .Opaque => true,
        else => false,
    }
}
