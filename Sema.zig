const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");
const types = @import("types.zig");
const Token = Tokenizer.Token;
const Type = types.Type;

structs: std.ArrayList(Struct),

pub const Struct = struct {
    name: [] const u8,

    pub const Field = struct {
        name: [] const u8,
        bitOffset: usize,
        @"type": Type,
    }; 

    pub const Idx = enum (u32) {
        _
    };
};