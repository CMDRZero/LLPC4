const std = @import("std");
const Trie = @import("Trie.zig").Trie;

pub const SymbolTable = struct {
    alloc: std.mem.Allocator,
    table: std.ArrayList(Data),
    typeTrie: Trie,
    endOfFrame: FrameIdx,

    pub fn init(alloc: std.mem.Allocator) SymbolTable {
        return .{
            .alloc = alloc,
            .table = .empty,
            .typeTrie = .init(alloc),
            .endOfFrame = .none,
        };
    }

    pub fn deinit(self: *SymbolTable) void {
        self.table.deinit(self.alloc);
    }

    const Data = union {
        item: Item,
        node: Node,
    };

    pub const Item = struct {
        name: Trie.ID,
        typeID: Trie.ID,
        ptr: *anyopaque,
    };

    pub const Node = struct {
        next: FrameIdx,
    };

    const FrameIdx = enum(u32){none = 0, _};
    
    pub const Idx = enum(u32){_};

    pub const Name = struct {
        name: Trie.ID,
    };

    pub const Reference = union (enum) {
        name: Name,
        idx: Idx,
    };

    fn nextFrame(self: SymbolTable, idx: usize) FrameIdx {
        return self.table.items[idx].node.next;
    }

    pub fn pushFrame(self: *SymbolTable) !void {
        const oldEndOfFrame = self.endOfFrame;
        self.table.append(self.alloc, .{.node = .{.next = oldEndOfFrame}});
        self.endOfFrame = @enumFromInt(self.table.items.len - 1);
    }

    pub fn popFrame(self: *SymbolTable) void {
        std.debug.assert(self.endOfFrame != .none);
        const idx = @intFromEnum(self.endOfFrame);
        self.endOfFrame = nextFrame(idx);
        self.table.shrinkRetainingCapacity(idx);
    }

    fn idxIsType(self: SymbolTable, idx: Idx, T: type) bool {
        const expectedId = self.typeTrie.lookup(@typeName(T)) orelse return false;
        return self.getItem(idx).typeID == expectedId;
    }

    fn getItem(self: SymbolTable, idx: Idx) Item {
        return self.table.items[@intFromEnum(idx)].item;
    }

    fn getItemPtr(self: *SymbolTable, idx: Idx) *Item {
        return &self.table.items[@intFromEnum(idx)].item;
    }


    pub fn getPtr(self: *const SymbolTable, idx: Idx, T: type) ?*T {
        if (!self.idxIsType(idx, T)) return null;
        return @ptrCast(@alignCast(@constCast(self).getItemPtr(idx).ptr));
    }

    pub fn setPtr(self: *SymbolTable, idx: Idx, ptr: anytype) void {
        self.getItemPtr(idx).ptr = ptr;
    }

    pub fn getValue(self: SymbolTable, idx: Idx, T: type) ?T {
        return (self.getPtr(idx, T) orelse return null).*;
    }

    pub fn setValue(self: SymbolTable, idx: Idx, T: type, value: T) void {
        self.getPtr(idx, T).?.* = value;
    }

    fn findName(self: SymbolTable, name: Trie.ID) ?Idx {
        var nextFrameHead: usize = @intFromEnum(self.endOfFrame);
        for (0..self.table.items.len) |i_| {
            const i = self.table.items.len - i_ - 1;
            if (i != 0 and i == nextFrameHead) {
                nextFrameHead = @intFromEnum(self.nextFrame(nextFrameHead));
                continue;
            }

            if (self.table.items[i].item.name == name) return @enumFromInt(i);
        } else {
            return null;
        }
    }

    pub fn toIdx(self: SymbolTable, ref: Reference) !Idx {
        if (ref == .idx) return ref.idx;
        return self.findName(ref.name.name) orelse return error.Reference_Doesnt_Exist;
    }

    pub fn refName(name: Trie.ID) Reference {
        return .{.name = .{.name = name}};
    }

    pub fn addValueAndRef(self: *SymbolTable, value: anytype) !Reference {
        return self.addNamedValueAndRef(.none, value);
    }

    pub fn addNamedValueAndRef(self: *SymbolTable, name: Trie.ID, value: anytype) !Reference {
        const T = @TypeOf(value);
        const valuePtr = try self.alloc.create(T);
        valuePtr.* = value;
        const item: Item = .{
            .name = name,
            .typeID = try self.typeTrie.writeGet(@typeName(T)),
            .ptr = @ptrCast(valuePtr),
        };
        try self.table.append(self.alloc, .{.item = item});
        return .{.idx = @enumFromInt(self.table.items.len - 1)};
    }

    pub fn getName(self: SymbolTable, idx: Idx) Trie.ID {
        return self.getItem(idx).name;
    }
    
    pub fn getNameStr(self: SymbolTable, name: Trie.ID) [] const u8 {
        return self.typeTrie.getString(name).?;
    }

    pub fn typeName(self: SymbolTable, idx: Idx) [] const u8 {
        return self.typeTrie.getString(self.getItem(idx).typeID).?;
    }

    pub fn assignNamedValue(self: *SymbolTable, name: Trie.ID, value: anytype) !void {
        if (self.findName(name)) |idx| {
            self.setValue(idx, @TypeOf(value), value);
        } else {
            _ = try self.addNamedValueAndRef(name, value);
        }
    }
};

