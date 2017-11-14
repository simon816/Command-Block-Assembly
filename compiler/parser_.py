from .nodes import *

# Mostly based on syntax specified in:
# https://github.com/antlr/grammars-v4/blob/master/c/C.g4

class Parser:

    def __init__(self, lexer):
        self.lexer = lexer

    def parse_program(self):
        stmts = []
        while self.lexer.next != Token.EOF:
            stmts.append(self.parse_external_decl())
        return stmts

    def parse_external_decl(self):
        # TODO using exceptions is bad
        pos, next = self.lexer.ptr, self.lexer.next
        try:
            return self.parse_declaration(allow_functions=True)
        except AssertionError as e:
            self.lexer.ptr, self.lexer.next = pos, next
            return self.parse_func_decl()

    def parse_func_decl(self):
        type = self.parse_decl_specifier()
        decl = self.parse_declarator()
        assert isinstance(decl.name_spec, FuncDeclSpec)
        body = self.parse_block()
        return FunctionDeclaration(body=body, type=type, decl=decl)

    def parse_declaration(self, allow_functions=False):
        type_spec = self.parse_decl_specifier(allow_struct_decl=True)
        initializers = []
        if self.lexer.next != Token.SEMICOLON:
            initializers = self.parse_init_declarator_list(allow_functions)
        self.read(Token.SEMICOLON)
        return Declaration(type=type_spec, init=initializers)

    def parse_init_declarator_list(self, allow_functions=False):
        inits = []
        while True:
            decl = self.parse_declarator()
            assert allow_functions or \
                   not isinstance(decl.name_spec, FuncDeclSpec)
            val = None
            if self.read_optional(Token.OP_ASSIGN):
                val = self.parse_initializer()
            inits.append(InitSpec(decl=decl, val=val))
            if not self.read_optional(Token.COMMA):
                break
        return inits

    def parse_initializer(self):
        if self.read_optional(Token.OPEN_BRACE):
            init = self.parse_initializer_list()
            self.read(Token.CLOSE_BRACE)
        else:
            init = self.parse_assignment_expr()
        return init

    def parse_initializer_list(self):
        inits = []
        while True:
            member_ref = None
            read_designator = False
            while True:
                if self.read_optional(Token.OPEN_SQUARE):
                    idx = self.parse_constant_expr()
                    self.read(Token.CLOSE_SQUARE)
                    member_ref = MemberReference(parent=member_ref, idx=idx)
                elif self.read_optional(Token.DOT):
                    member = self.parse_identifier()
                    member_ref = MemberReference(parent=member_ref, name=member)
                else:
                    break
                read_designator = True

            if read_designator:
                self.read(Token.OP_ASSIGN)

            val = self.parse_initializer()
            inits.append(InitSpec(decl=member_ref, val=val))
            if not self.read_optional(Token.COMMA):
                break
            # Allow for trailing comma
            if self.lexer.next == Token.CLOSE_BRACE:
                break
        return inits

    def parse_declarator(self):
        pointers = 0
        while self.read_optional(Token.OP_STAR):
            pointers += 1
        decl = self.parse_direct_declarator()
        return DeclaratorSpec(pointer_depth=pointers, name_spec=decl)

    def parse_direct_declarator(self):
        name = self.parse_identifier()
        if self.read_optional(Token.OPEN_SQUARE):
            array_dim = None
            if not self.read_optional(Token.CLOSE_SQUARE):
                array_dim = self.parse_assignment_expr()
                self.read(Token.CLOSE_SQUARE)
            return ArrayDeclSpec(name=name, dim=array_dim)

        elif self.read_optional(Token.OPEN_PAREN):
            params = []
            if self.lexer.next != Token.CLOSE_PAREN:
                params = self.parse_param_list()
            self.read(Token.CLOSE_PAREN)
            return FuncDeclSpec(name=name, params=params)
        else:
            return name

    def parse_param_list(self):
        # varargs would go here if supported
        params = [self.parse_param_decl()]
        while self.read_optional(Token.COMMA):
            params.append(self.parse_param_decl())
        return params

    def parse_param_decl(self):
        type = self.parse_decl_specifier()
        decl = None
        def is_just_pointers_ahead():
            # this is ew
            pos, next = self.lexer.ptr, self.lexer.next
            while self.read_optional(Token.OP_STAR):
                if self.lexer.next in [Token.COMMA, Token.CLOSE_PAREN]:
                    break
            ret = self.lexer.next in [Token.COMMA, Token.CLOSE_PAREN]
            self.lexer.ptr, self.lexer.next = pos, next
            return ret
        # Could be an unnamed param
        if self.lexer.next not in [Token.COMMA, Token.CLOSE_PAREN] and \
           not is_just_pointers_ahead():
            decl = self.parse_declarator()
        else:
            # allow for pointers
            pointers = 0
            while self.read_optional(Token.OP_STAR):
                pointers += 1
            decl = DeclaratorSpec(pointer_depth=pointers)
        return ParamDeclaration(type=type, decl=decl)

    def parse_decl_specifier(self, allow_struct_decl=False):
        store = self.read_optional_any(Keyword.TYPEDEF, Keyword.STATIC)
        qualifier = self.read_optional_any(Keyword.CONST)
        type = self.parse_type_specifier(allow_struct_decl)
        return DeclarationSpecifier(store=store, qual=qualifier, type=type)

    def can_next_be_type_spec(self):
        return self.lexer.next in [Keyword.STRUCT] or \
           self.lexer.next.type == Token.Type.IDENTIFIER

    def parse_type_specifier(self, allow_struct_decl=False):
        if self.read_optional(Keyword.STRUCT):
            def is_declaration_ahead():
                if self.lexer.next_next() == Token.SEMICOLON:
                    return False
                if self.lexer.next == Token.OPEN_BRACE:
                    return True
                if self.lexer.next_next() == Token.OP_STAR:
                    return False
                if self.lexer.next_next().type == Token.Type.IDENTIFIER:
                    return False
                return True
            if not allow_struct_decl or not is_declaration_ahead():
                return StructTypeRef(name=self.parse_identifier())
            name = None
            if not self.read_optional(Token.OPEN_BRACE):
                name = self.parse_identifier()
                self.read(Token.OPEN_BRACE)
            decl_list = self.parse_struct_declaration_list()
            self.read(Token.CLOSE_BRACE)
            return StructSpec(name=name, decl=decl_list)
        # TODO enumSpecifier
        # typedefName
        if self.lexer.next.type == Token.Type.IDENTIFIER:
            return self.parse_identifier()
        assert False

    def parse_struct_declaration_list(self):
        decls = []
        while True:
            type = self.parse_type_specifier(allow_struct_decl=True)
            decl = []
            if not self.read_optional(Token.SEMICOLON):
                decl = self.parse_struct_declarator_list()
                self.read(Token.SEMICOLON)
            decls.append(StructMemberDecl(spec=type, decl=decl))
            if self.lexer.next != Keyword.CONST and \
               not self.can_next_be_type_spec():
                break
        return decls

    def parse_struct_declarator_list(self):
        decls = [self.parse_struct_decl()]
        while self.read_optional(Token.COMMA):
            decls.append(self.parse_struct_decl())
        return decls

    def parse_struct_decl(self):
        return self.parse_declarator()

    def parse_type_name(self):
        type = self.parse_type_specifier()
        # Allow for pointers
        pointers = 0
        while self.read_optional(Token.OP_STAR):
            pointers += 1
        return TypeName(type=type, spec=DeclaratorSpec(pointer_depth=pointers))

    def parse_block(self):
        block = []
        self.read(Token.OPEN_BRACE)
        while not self.read_optional(Token.CLOSE_BRACE):
            block.append(self.parse_block_item())
        return block

    def parse_block_item(self):
        # TODO using exceptions is bad
        pos, next = self.lexer.ptr, self.lexer.next
        try:
            assert next != Keyword.SYNC # quick hack
            return self.parse_declaration(allow_functions=False)
        except AssertionError as e:
            self.lexer.ptr, self.lexer.next = pos, next
            return self.parse_statement()

    def parse_statement(self):
        next = self.lexer.next
        if next == Token.SEMICOLON:
            self.next()
            return EmptyStatement.INSTANCE
        elif next == Token.OPEN_BRACE:
            return self.parse_block()
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
                if next_next == Token.COLON:
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
                cases.append(self.parse_case(True))
            else:
                assert False
        return SwitchStmt(expr=expr, cases=cases)

    def parse_case(self, is_default=False):
        if is_default:
            self.read(Keyword.DEFAULT)
            expr = None
        else:
            self.read(Keyword.CASE)
            expr = self.parse_constant_expr()
        self.read(Token.COLON)
        body = []
        allow_brace = True
        while True:
            if self.lexer.next in [Keyword.CASE, Keyword.DEFAULT]:
                break
            elif body and self.lexer.next == Token.CLOSE_BRACE:
                break
            else:
                stmt = self.parse_statement()
                if type(stmt) == list:
                    assert allow_brace
                    body = stmt
                    break
                allow_brace = False
                body.append(stmt)
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
        true = self.parse_statement()
        false = []
        if self.lexer.next == Keyword.ELSE:
            self.read(Keyword.ELSE)
            false = self.parse_statement()
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
        body = self.parse_statement()
        return ForStmt(init=init, cond=cond, after=after, body=body)

    def parse_while(self):
        self.read(Keyword.WHILE)
        self.read(Token.OPEN_PAREN)
        cond = self.parse_expression()
        self.read(Token.CLOSE_PAREN)
        body = self.parse_statement()
        return WhileStmt(cond=cond, body=body)

    def parse_sync(self):
        self.read(Keyword.SYNC)
        self.read(Keyword.SEMICOLON)
        return SyncStmt()

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
            left = BinaryOperatorExpr(left=left,op=Token.OP_OR_OR.val,right=right)
        return left

    def parse_logic_and_expr(self):
        left = self.parse_inclusive_or_expr()
        while self.read_optional(Token.OP_AND_AND):
            right = self.parse_inclusive_or_expr()
            left = BinaryOperatorExpr(left=left,op=Token.OP_AND_AND.val,right=right)
        return left

    def parse_inclusive_or_expr(self):
        left = self.parse_xor_expr()
        while self.read_optional(Token.OP_OR):
            right = self.parse_xor_expr()
            left = BinaryOperatorExpr(left=left, op=Token.OP_OR.val, right=right)
        return left

    def parse_xor_expr(self):
        left = self.parse_and_expr()
        while self.read_optional(Token.OP_XOR):
            right = self.parse_and_expr()
            left = BinaryOperatorExpr(left=left, op=Token.OP_XOR.val, right=right)
        return left

    def parse_and_expr(self):
        left = self.parse_equality_expr()
        while self.read_optional(Token.OP_AND):
            right = self.parse_equality_expr()
            left = BinaryOperatorExpr(left=left, op=Token.OP_AND.val, right=right)
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
            Token.OP_AND, Token.OP_STAR, Token.OP_PLUS,
            Token.OP_MINUS, Token.OP_BITNOT, Token.OP_NOT
        ]:
            op = self.next().val
            return UnaryExpr(op=op, expr=self.parse_cast_expr())
        elif self.lexer.next in [Token.OP_PLUS_PLUS, Token.OP_MINUS_MINUS]:
            op = self.next().val
            expr = self.parse_unary_expr()
            if op == '++':
                return IncrementExpr(dir=1, post=False, expr=expr)
            elif op == '--':
                return IncrementExpr(dir=-1, post=False, expr=expr)
            assert False
        elif self.lexer.next == Keyword.SIZEOF:
            self.read(Keyword.SIZEOF)
            if self.read_optional(Token.OPEN_PAREN):
                expr = self.parse_type_name()
                self.read(Token.CLOSE_PAREN)
            else:
                expr = self.parse_unary_expr()
            return SizeofExpr(expr=expr)
        else:
            return self.parse_postfix_expr()

    def parse_postfix_expr(self):
        left = self.parse_primary_expr()

        while True:
            if self.read_optional(Token.OPEN_SQUARE):
                right = self.parse_expression()
                self.read(Token.CLOSE_SQUARE)
                left = ArraySubscriptExpr(expr=left, sub=right)

            elif self.read_optional(Token.OPEN_PAREN):
                args = []
                while not self.read_optional(Token.CLOSE_PAREN):
                    args.append(self.parse_assignment_expr())
                    if not self.read_optional(Token.COMMA):
                        self.read(Token.CLOSE_PAREN)
                        break
                left = FunctionCallExpr(ref=left, args=args)

            elif self.read_optional(Token.DOT):
                prop = self.parse_identifier()
                left = MemberAccessExpr(expr=left, prop=prop, deref=False)
            elif self.read_optional(Token.ARROW):
                prop = self.parse_identifier()
                left = MemberAccessExpr(expr=left, prop=prop, deref=True)

            elif self.read_optional(Token.OP_PLUS_PLUS):
                left = IncrementExpr(dir=1, post=True, expr=left)
            elif self.read_optional(Token.OP_MINUS_MINUS):
                left = IncrementExpr(dir=-1, post=True, expr=left)

            else:
                break

        return left


    def parse_primary_expr(self):
        next = self.lexer.next
        if next == Token.OPEN_PAREN:
            self.read(Token.OPEN_PAREN)
            expr = self.parse_expression()
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

    def can_next_be_expr(self):
        next = self.lexer.next
        return next in [Token.OPEN_PAREN, Token.CLOSE_PAREN] or \
               next.type in [Token.Type.IDENTIFIER, Token.Type.NUMBER,
                             Token.Type.STRING]

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

    def read_optional_any(self, *tokens):
        if self.lexer.next in tokens:
            return self.lexer.next_token()
        return None
