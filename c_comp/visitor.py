from .nodes import *

class Visitor:

    def __init__(self):
        self.after_next = None
        self.expr_func_map = {
            AssignmentExpr: self.visit_assign_expr,
            IncrementExpr: self.visit_increment_expr,
            FunctionCallExpr: self.visit_func_call_expr,
            IdentifierExpr: self.visit_var_expr,
            IntLiteral: self.visit_int_literal_expr,
            StringLiteral: self.visit_string_literal_expr,
            AssignmentOperatorExpr: self.visit_assign_op_expr,
            BinaryOperatorExpr: self.visit_binop_expr,
            MemberAccessExpr: self.visit_member_access_expr,
            SizeofExpr: self.visit_sizeof_expr,
            ConditionalExpr: self.visit_conditional_expr,
            UnaryExpr: self.visit_unary_expr,
            ArraySubscriptExpr: self.visit_arr_subscript_expr,
        }
        self.stmt_func_map = {
            Declaration: self.visit_decl,

            ExpressionStmt: self.visit_expr_stmt,

            WhileStmt: self.visit_while_stmt,

            DoWhileStmt: self.visit_do_while_stmt,
            ForStmt: self.visit_for_stmt,
            IfStmt: self.visit_if_stmt,

            ContinueStmt: self.visit_continue_stmt,
            GotoStmt: self.visit_goto_stmt,
            BreakStmt: self.visit_break_stmt,
            LabelledStmt: self.visit_labelled_stmt,

            SwitchStmt: self.visit_switch_stmt,

            ReturnStmt: self.visit_return_stmt,
            SyncStmt: self.visit_sync_stmt,
        }

    def visit_program(self, program):
        for decl in program:
            if isinstance(decl, Pragma):
                self.visit_pragma(decl)
                continue
            self.visit_declaration(decl)

    def visit_pragma(self, pragma):
        pass

    def visit_declaration(self, decl):
        if isinstance(decl, Declaration):
            return self.visit_decl(decl)
        elif isinstance(decl, FunctionDeclaration):
            return self.visit_func_decl(decl)
        else:
            assert False, 'Unknown declaration type %s' % decl.__class__.__name__

    def after_next_statement(self, ir):
        self.after_next = ir

    def visit_statements(self, stmts):
        for stmt in stmts:
            self.visit_statement(stmt)

    def visit_statement(self, stmt):
        prev_after = self.after_next
        if type(stmt) == list:
            self.after_next = None
            self.visit_statements(stmt)
            self.after_next = prev_after
        elif isinstance(stmt, EmptyStatement):
            pass
        elif isinstance(stmt, Pragma):
            self.visit_pragma(stmt)
        else:
            func = self.stmt_func_map.get(type(stmt))
            assert func is not None, 'Unknown statement type %s' % stmt.__class__.__name__
            func(stmt)
        if self.after_next:
            if prev_after:
                prev_after()
                if self.after_next == prev_after:
                    prev_after = None
                self.after_next = prev_after

    def visit_expression(self, expr):
        func = self.expr_func_map.get(type(expr))
        assert func is not None, 'Unknown expression type %s' % expr.__class__.__name__
        return func(expr)

    def visit_decl(self, stmt):
        pass

    def visit_expr_stmt(self, stmt):
        pass

    def visit_while_stmt(self, stmt):
        pass

    def visit_do_while_stmt(self, stmt):
        pass

    def visit_for_stmt(self, stmt):
        pass

    def visit_if_stmt(self, stmt):
        pass

    def visit_continue_stmt(self, stmt):
        pass

    def visit_goto_stmt(self, stmt):
        pass

    def visit_break_stmt(self, stmt):
        pass

    def visit_labelled_stmt(self, stmt):
        pass

    def visit_switch_stmt(self, stmt):
        pass

    def visit_return_stmt(self, stmt):
        pass

    def visit_sync_stmt(self, stmt):
        pass

    def visit_assign_expr(self, expr):
        pass

    def visit_increment_expr(self, expr):
        pass

    def visit_func_call_expr(self, expr):
        pass

    def visit_var_expr(self, expr):
        pass

    def visit_string_literal_expr(self, expr):
        pass

    def visit_int_literal_expr(self, expr):
        pass

    def visit_assign_op_expr(self, expr):
        pass

    def visit_binop_expr(self, expr):
        pass

    def visit_member_access_expr(self, expr):
        pass

    def visit_sizeof_expr(self, expr):
        pass

    def visit_conditional_expr(self, expr):
        pass

    def visit_unary_expr(self, expr):
        pass

    def visit_arr_subscript_expr(self, expr):
        pass

