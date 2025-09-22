const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");

const Token = Tokenizer.Token;

pub const Node = struct {
    main_token: Token,
    data: Data,

    pub const Data = union (enum) {
        op_binary: struct {
            result_type: ?void,
            lhs: *Node,
            rhs: *Node,
        },
        op_prefix_unary: struct {
            result_type: ?void,
            rhs: *Node,
        },
        op_suffix_unary: struct {
            result_type: ?void,
            lhs: *Node,
        },
    }; 
};