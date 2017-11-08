from .nodes import *

class Parser:

    def __init__(self, lexer):
        self.lexer = lexer

    def parse_program(self):
        stmts = []
        while self.lexer.next != Token.EOF:
            stmts.append(self.parse_func())
        return stmts

    def parse_func(self):
        type = self.parse_identifier()
        name = self.parse_identifier()
        self.read(Token.OPEN_PAREN)
        params = []
        while self.lexer.next != Token.CLOSE_PAREN:
            p_type = self.parse_identifier()
            p_name = self.parse_identifier()
            self.read_optional(Token.COMMA)
            params.append(FunctionArg(type=p_type, name=p_name))
        self.read(Token.CLOSE_PAREN)
        body = self.parse_block()
        return FunctionDeclaration(ret_type=type, name=name, params=params,
                                   body=body)

    def parse_block(self):
        block = []
        self.read(Token.OPEN_BRACE)
        while not self.read_optional(Token.CLOSE_BRACE):
            block.append(self.parse_statement())
        return block

    def parse_statement(self):
        next = self.lexer.next
        if next == Token.SEMICOLON:
            self.next()
            return EmptyStatement.INSTANCE
        elif next == Keyword.DO:
            return self.parse_do_while()
        elif next == Keyword.FOR:
            return self.parse_for()
        elif next == Keyword.IF:
            return self.parse_if()
        elif next == Keyword.WHILE:
            return self.parse_while()
        elif next == Keyword.SYNC:
            return self.parse_sync()
        elif next == Keyword.SWITCH:
            return self.parse_switch()
        elif next == Keyword.GOTO:
            return self.parse_goto()
        elif next == Keyword.CONTINUE:
            return self.parse_continue()
        elif next == Keyword.BREAK:
            return self.parse_break()
        elif next == Keyword.RETURN:
            return self.parse_return()
        else:
            if next.type == Token.Type.IDENTIFIER:
                next_next = self.lexer.next_next()
                if next_next.type == Token.Type.IDENTIFIER:
                    return self.parse_declaration()
                elif next_next == Token.COLON:
                    return self.parse_label_stmt()
            stmt = ExpressionStmt(expr=self.parse_expression())
            self.read(Token.SEMICOLON)
            return stmt

    def parse_label_stmt(self):
        label = self.parse_identifier()
        self.read(Token.COLON)
        stmt = self.parse_statement()
        return LabelledStmt(label=label, stmt=stmt)

    def parse_switch(self):
        self.read(Keyword.SWITCH)
        self.read(Token.OPEN_PAREN)
        expr = self.parse_expression()
        self.read(Token.CLOSE_PAREN)
        self.read(Token.OPEN_BRACE)
        cases = []
        seen_default = False
        while not self.read_optional(Token.CLOSE_BRACE):
            if self.lexer.next == Keyword.CASE:
                cases.append(self.parse_case())
            elif self.lexer.next == Keyword.DEFAULT:
                assert not seen_default
                seen_default = True
                self.read(Keyword.DEFAULT)
                self.read(Token.COLON)
                cases.append(CaseStmt(body=self.parse_block()))
        return SwitchStmt(expr=expr, cases=cases)

    def parse_case(self):
        self.read(Keyword.CASE)
        expr = self.parse_constant_expr()
        self.read(Token.COLON)
        body = self.parse_block()
        return CaseStmt(choice=expr, body=body)

    def parse_constant_expr(self):
        return self.parse_conditional_expr()

    def parse_goto(self):
        self.read(Keyword.GOTO)
        label = self.parse_identifier()
        self.read(Token.SEMICOLON)
        return GotoStmt(label=label)

    def parse_continue(self):
        self.read(Keyword.CONTINUE)
        self.read(Token.SEMICOLON)
        return ContinueStmt.INSTANCE

    def parse_break(self):
        self.read(Keyword.BREAK)
        self.read(Token.SEMICOLON)
        return BreakStmt.INSTANCE

    def parse_return(self):
        self.read(Keyword.RETURN)
        expr = None
        if self.lexer.next != Token.SEMICOLON:
            expr = self.parse_expression()
        self.read(Token.SEMICOLON)
        return ReturnStmt(expr=expr)

    def parse_if(self):
        self.read(Keyword.IF)
        self.read(Token.OPEN_PAREN)
        cond = self.parse_expression()
        self.read(Token.CLOSE_PAREN)
        true = self.parse_block()
        false = []
        if self.lexer.next == Keyword.ELSE:
            self.read(Keyword.ELSE)
            false = self.parse_block()
        return IfStmt(cond=cond, true=true, false=false)

    def parse_do_while(self):
        self.read(Keyword.DO)
        body = self.parse_block()
        self.read(Keyword.WHILE)
        self.read(Token.OPEN_PAREN)
        expr = self.parse_expression()
        self.read(Token.CLOSE_PAREN)
        self.read(Token.SEMICOLON)
        return DoWhileStmt(body=body, cond=expr)

    def parse_for(self):
        self.read(Keyword.FOR)
        self.read(Token.OPEN_PAREN)

        init = None
        if self.lexer.next != Token.SEMICOLON:
            init = self.parse_expression()
        self.read(Token.SEMICOLON)

        cond = None
        if self.lexer.next != Token.SEMICOLON:
            cond = self.parse_expression()
        self.read(Token.SEMICOLON)

        after = None
        if self.lexer.next != Token.CLOSE_PAREN:
            after = self.parse_expression()

        self.read(Token.CLOSE_PAREN)
        body = self.parse_block()
        return ForStmt(init=init, cond=cond, after=after, body=body)

    def parse_while(self):
        self.read(Keyword.WHILE)
        self.read(Token.OPEN_PAREN)
        cond = self.parse_expression()
        self.read(Token.CLOSE_PAREN)
        body = self.parse_block()
        return WhileStmt(cond=cond, body=body)

    def parse_sync(self):
        self.read(Keyword.SYNC)
        self.read(Keyword.SEMICOLON)
        return SyncStmt()

    def parse_declaration(self):
        type = self.parse_identifier()
        name = self.parse_identifier()
        init = None
        if self.lexer.next == Token.OP_ASSIGN:
            self.read(Token.OP_ASSIGN)
            init = self.parse_expression()
        self.read(Token.SEMICOLON)
        return VariableDeclaration(type=type, name=name, init=init)

    def parse_expression(self):
        left = self.parse_assignment_expr()
        # Left associate
        while self.read_optional(Token.COMMA):
            left = BinaryOperationExpr(left=left,op=Token.COMMA,
                                       right=self.parse_assignment_expr())
        return left

    def parse_assignment_expr(self):
        left = self.parse_conditional_expr()
        # Right associate
        if self.read_optional(Token.OP_ASSIGN):
            right = self.parse_assignment_expr()
            return AssignmentExpr(left=left, right=right)
        elif self.lexer.next in [
            Token.OP_MUL_ASSIGN,
            Token.OP_DIV_ASSIGN,
            Token.OP_MOD_ASSIGN,
            Token.OP_PLUS_ASSIGN,
            Token.OP_MINUS_ASSIGN,
            Token.OP_LSHIFT_ASSIGN,
            Token.OP_RSHIFT_ASSIGN,
            Token.OP_AND_ASSIGN,
            Token.OP_XOR_ASSIGN,
            Token.OP_OR_ASSIGN
        ]:
            op = self.next().val
            right = self.parse_assignment_expr()
            return AssignmentOperatorExpr(left=left, op=op, right=right)
        return left

    def parse_conditional_expr(self):
        expr = self.parse_logic_or_expr()
        if self.read_optional(Token.QUESTION):
            true = self.parse_expression()
            self.read(Token.COLON)
            false = self.parse_conditional_expr()
            return ConditionalExpr(cond=expr, true=true, false=false)
        return expr

    def parse_logic_or_expr(self):
        left = self.parse_logic_and_expr()
        while self.read_optional(Token.OP_OR_OR):
            right = self.parse_logic_and_expr()
            left = BinaryOperatorExpr(left=left,op=Token.OP_OR_OR,right=right)
        return left

    def parse_logic_and_expr(self):
        left = self.parse_inclusive_or_expr()
        while self.read_optional(Token.OP_AND_AND):
            right = self.parse_inclusive_or_expr()
            left = BinaryOperatorExpr(left=left,op=Token.OP_AND_AND,right=right)
        return left

    def parse_inclusive_or_expr(self):
        left = self.parse_xor_expr()
        while self.read_optional(Token.OP_OR):
            right = self.parse_xor_expr()
            left = BinaryOperatorExpr(left=left, op=Token.OP_OR, right=right)
        return left

    def parse_xor_expr(self):
        left = self.parse_and_expr()
        while self.read_optional(Token.OP_XOR):
            right = self.parse_and_expr()
            left = BinaryOperatorExpr(left=left, op=Token.OP_XOR, right=right)
        return left

    def parse_and_expr(self):
        left = self.parse_equality_expr()
        while self.read_optional(Token.OP_AND):
            right = self.parse_equality_expr()
            left = BinaryOperatorExpr(left=left, op=Token.OP_AND, right=right)
        return left

    def parse_equality_expr(self):
        left = self.parse_relational_expr()
        while self.lexer.next in [Token.OP_EQUAL, Token.OP_NOT_EQUAL]:
            op = self.next().val
            right = self.parse_relational_expr()
            left = BinaryOperatorExpr(left=left, op=op, right=right)
        return left

    def parse_relational_expr(self):
        left = self.parse_shift_expr()
        while self.lexer.next in [
            Token.OP_LESS_THAN, Token.OP_LESS_OR_EQUAL,
            Token.OP_GREATER_THAN, Token.OP_GREATER_OR_EQUAL
        ]:
            op = self.next().val
            right = self.parse_shift_expr()
            left = BinaryOperatorExpr(left=left, op=op, right=right)
        return left

    def parse_shift_expr(self):
        left = self.parse_add_expr()
        while self.lexer.next in [Token.OP_SHIFT_LEFT, Token.OP_SHIFT_RIGHT]:
            op = self.next().val
            right = self.parse_add_expr()
            left = BinaryOperatorExpr(left=left, op=op, right=right)
        return left

    def parse_add_expr(self):
        left = self.parse_mul_expr()
        while self.lexer.next in [Token.OP_PLUS, Token.OP_MINUS]:
            op = self.next().val
            right = self.parse_mul_expr()
            left = BinaryOperatorExpr(left=left, op=op, right=right)
        return left

    def parse_mul_expr(self):
        left = self.parse_cast_expr()
        while self.lexer.next in [Token.OP_STAR, Token.OP_DIV, Token.OP_MOD]:
            op = self.next().val
            right = self.parse_cast_expr()
            left = BinaryOperatorExpr(left=left, op=op, right=right)
        return left

    def parse_cast_expr(self):
        expr = self.parse_unary_expr()
        # TODO
        return expr

    def parse_unary_expr(self):
        if self.lexer.next in [
            Token.OP_PLUS_PLUS, Token.OP_MINUS_MINUS,
            Token.OP_AND, Token.OP_STAR, Token.OP_PLUS,
            Token.OP_MINUS, Token.OP_BITNOT, Token.OP_NOT
        ]:
            op = self.next().val
            expr = self.parse_unary_expr()
            if op == '++':
                return IncrementExpr(dir=1, post=False, expr=expr)
            elif op == '--':
                return IncrementExpr(dir=-1, post=False, expr=expr)
            else:
                return UnaryExpr(op=op, expr=expr)
        elif self.lexer.next == Keyword.SIZEOF:
            if self.read_optional(Token.OPEN_PAREN):
                expr = self.parse_identifier()
                self.read(Token.CLOSE_PAREN)
            else:
                expr = self.parse_unary_expr()
            return SizeofExpr(expr=expr)
        else:
            return self.parse_postfix_expr()

    def parse_postfix_expr(self):
        expr = self.parse_primary_expr()
        if self.read_optional(Token.OPEN_SQUARE):
            pass # TODO
        elif self.read_optional(Token.OPEN_PAREN):
            args = []
            while not self.read_optional(Token.CLOSE_PAREN):
                args.append(self.parse_assignment_expr())
                if not self.read_optional(Token.COMMA):
                    self.read(Token.CLOSE_PAREN)
                    break
            return FunctionCallExpr(ref=expr, args=args)
        elif self.read_optional(Token.DOT):
            pass # TODO
        elif self.read_optional(Token.ARROW):
            pass # TODO
        elif self.read_optional(Token.OP_PLUS_PLUS):
            return IncrementExpr(dir=1, post=True, expr=expr)
        elif self.read_optional(Token.OP_MINUS_MINUS):
            return IncrementExpr(dir=-1, post=True, expr=expr)
        return expr

    def parse_primary_expr(self):
        next = self.lexer.next
        if next == Token.OPEN_PAREN:
            expr = self.self.parse_expression()
            self.read(Token.CLOSE_PAREN)
            return expr
        elif next.type == Token.Type.IDENTIFIER:
            return self.parse_identifier()
        elif next.type == Token.Type.NUMBER:
            return self.parse_number()
        elif next.type == Token.Type.STRING:
            return self.parse_string()
        else:
            assert False, 'expected number or identifier, got %r' % next.val


    def parse_identifier(self):
        token = self.lexer.next_token()
        assert token.type == Token.Type.IDENTIFIER
        return IdentifierExpr(val=token.val)

    def parse_number(self):
        return IntLiteral(val=int(self.next().val))

    def parse_string(self):
        return StringLiteral(val=str(self.next().val))

    def read(self, token):
        next = self.lexer.next_token()
        assert next == token, "Expected %s, found %s" % (token.val, next.val)

    def next(self):
        return self.lexer.next_token()

    def read_optional(self, token):
        if self.lexer.next == token:
            self.lexer.next_token()
            return True
        return False
