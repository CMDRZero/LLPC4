const std = @import("std");
const Trie = @import("Trie.zig").Trie;

pub fn Symbol(T: type) type {
    return struct {
        data: T,
        isDefined: bool = false,
    };
}

pub fn parentPtr(ptr: anytype) *Symbol(std.meta.Child(@TypeOf(ptr))) {
    const ptrT = @TypeOf(ptr);
    const T = std.meta.Child(ptrT);
    return @as(*Symbol(T), @fieldParentPtr("data", ptr));
}

pub fn checkIsDefined(ptr: anytype) bool {
    return parentPtr(ptr).isDefined;
}

pub fn setDefined(ptr: anytype, value: bool) void {
    parentPtr(ptr).isDefined = value;
}

pub const Table = struct {
    pub const Item = struct {
        name: Trie.ID,
        ptr: *anyopaque,
    };

    table: std.ArrayList(Item),

    pub const empty: Table = .{.table = .empty};

    pub fn deinit(self: *Table, gpa: std.mem.Allocator) void {
        self.table.deinit(gpa);
    }

    pub fn Idx(T: type) type {
        return struct {
            const resPtrT: type = *T;
            const resT: type = T;
            value: u32,
        };
    }

    pub fn getValue(self: Table, idx: anytype) !@TypeOf(idx).resT {
        const ptr = self.getPtr(idx);
        if (!checkIsDefined(ptr)) return error.Undefined_Symbol;
        return ptr.*;
    }

    pub fn setValue(self: Table, idx: anytype, value: @TypeOf(idx).resT) void {
        const ptr = self.getPtr(idx);
        setDefined(ptr, true);
        ptr.* = value;
    }

    pub fn isSet(self: Table, idx: anytype) bool {
        return checkIsDefined(self.getPtr(idx));
    } 

    pub fn getPtr(self: Table, idx: anytype) @TypeOf(idx).resPtrT {
        return @ptrCast(@alignCast(self.table.items[idx.value].ptr));
    }

    pub fn setPtr(self: *Table, idx: anytype, value: @TypeOf(idx).resPtrT) void {
        self.table.items[idx.value].ptr = @bitCast(value);
    }

    pub fn getName(self: Table, idx: anytype) Trie.ID {
        return self.table.items[idx.value].name;
    }

    pub fn setName(self: *Table, idx: anytype, name: Trie.ID) void {
        self.table.items[idx.value].name = name;
    }

    pub fn rawAppendItem(self: *Table, gpa: std.mem.Allocator, name: Trie.ID, ptrToItem: anytype) !void {
        try self.table.append(gpa, .{.ptr = ptrToItem, .name = name});
    }
    
    pub fn appendItem(self: *Table, gpa: std.mem.Allocator, name: Trie.ID, ptrToItem: anytype) !Table.Idx(std.meta.Child(@TypeOf(ptrToItem))) {
        try self.rawAppendItem(gpa, name, ptrToItem);
        return .{.value = @intCast(self.table.items.len - 1)};
    }

    pub fn appendNamedUndefined(self: *Table, gpa: std.mem.Allocator, name: Trie.ID, T: type) !Table.Idx(T) {
        const symPtr = try gpa.create(Symbol(T));
        return self.appendItem(gpa, name, &symPtr.data);
    }

    pub fn appendNamedValue(self: *Table, gpa: std.mem.Allocator, name: Trie.ID, value: anytype) !Table.Idx(@TypeOf(value)) {
        const T = @TypeOf(value);
        const idx = try self.appendNamedUndefined(gpa, name, T);
        self.setValue(idx, value);
        return idx;
    }

    pub fn appendUnnamedValue(self: *Table, gpa: std.mem.Allocator, value: anytype) !Table.Idx(@TypeOf(value)) {
        return self.appendNamedValue(gpa, .none, value);
    }
};