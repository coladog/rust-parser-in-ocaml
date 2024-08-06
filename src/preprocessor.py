#!/usr/bin/env python3
from enum import Enum
import re
import sys

class Preprocess():
    class Tokens(Enum):
        IF = r"\bif\b"
        ELSE = r"\belse\b"
        LPAREN = r"\("
        RPAREN = r"\)"
        LBRACE = r"\{"
        RBRACE = r"\}"
        LBRACK = r"\["
        RBRACK = r"\]"
    def __init__(self, s : str):
        T = Preprocess.Tokens
        self.s = s
        self.token_to_str = {
            T.IF : "if",
            T.ELSE : "else",
            T.LPAREN : "(",
            T.RPAREN : ")",
            T.LBRACE : "{",
            T.RBRACE : "}",
            T.LBRACK : "[",
            T.RBRACK : "]"
        }

        self.token_open_to_close = {
            T.LPAREN : T.RPAREN,
            T.LBRACE : T.RBRACE, 
            T.LBRACK : T.RBRACK,
            T.IF : T.ELSE
        }
        self.token_close_to_open = {v : k for k, v in self.token_open_to_close.items()}
        self.openings = set(self.token_open_to_close.keys())
        self.closings = set(self.token_open_to_close.values())
        self.tokens = []
        self.token_and_strs = []
        self.input_buffer = ""
        self.token_pos_to_match = []
        self.base_idx = 0
    def update_token(self, c : str):
        T = Preprocess.Tokens
        assert len(c) == 1
        self.input_buffer += c

        for t in T:
            m = re.search(t.value, self.input_buffer)
            if m:
                self.tokens.append(t)
                self.token_pos_to_match.append((m.start() + self.base_idx, m.end() + self.base_idx))
                self.input_buffer = self.input_buffer[m.end():]
                self.base_idx += m.end()
                return 
    def get_token_str_list(self):
        if len(self.tokens) > 0:
            if self.token_pos_to_match[0][0] > 0:
                self.token_and_strs.append(self.s[:self.token_pos_to_match[0][0]])
            for (i, t) in enumerate(self.tokens[:-1]):
                self.token_and_strs.append(t)
                new_s = self.s[self.token_pos_to_match[i][1]:self.token_pos_to_match[i+1][0]]
                if len(new_s) > 0:
                    self.token_and_strs.append(new_s)
            self.token_and_strs.append(self.tokens[-1])
            new_s = self.s[self.token_pos_to_match[-1][1]:]
            if len(new_s) > 0:
                self.token_and_strs.append(new_s)
        if len(self.input_buffer) > 0:
            self.token_and_strs.append(self.input_buffer)
        return self.token_and_strs
    
    def get_balance_list(self, token_and_str = None):
        if token_and_str is None: token_and_str = self.token_and_strs
        stack = []
        balance_list = [-1] * len(token_and_str)
        for i, t in enumerate(token_and_str):
            if t in self.openings:
                stack.append((t, i))
            elif t in self.closings:
                if len(stack) == 0:
                    balance_list[i:] = [False] * (len(token_and_str) - i)
                if len(stack) > 0 and self.token_open_to_close[stack[-1][0]] == t:
                    stack.pop()
                else:
                    balance_list[i:] = [False] * (len(token_and_str) - i)
                    
            balance_list[i] = len(stack) == 0
        return balance_list
    
    @staticmethod
    def find_next_true_pos(start_idx, lst):
        for i in range(start_idx, len(lst)):
            if lst[i]: return i
        return -1
    @staticmethod
    def find_last_true_pos(start_idx, lst):
        for i in range(start_idx, -1, -1):
            if lst[i]: return i
        return -1
    
    def find_next_token_pos(self, start_idx, token, lst = None):
        if lst is None: lst = self.token_and_strs
        for i in range(start_idx, len(lst)):
            if lst[i] == token: return i
        return -1
    
    def tokenize(self):
        for c in self.s:
            self.update_token(c)
        self.update_token("]")
        # forcing the lexer to update remaining contents 
        self.get_token_str_list()
        self.tokens.pop()
        self.token_and_strs.pop()
        return self.token_and_strs
    
    def process_tokenized(self):
        T = Preprocess.Tokens
        ts = self.token_and_strs
        for i in range(0, len(ts), 1):
            t = ts[i]
            if t != T.IF: continue
            ts = ts[:i + 1] + [T.LPAREN] + ts[i + 1:]
            # use temp to exclude if
            temp = ts[i+2:]
            temp_balance = self.get_balance_list(temp)
            for j, tj in enumerate(temp):
                if not temp_balance[j]: continue
                while True:
                    nex_lb_pos = self.find_next_token_pos(j, T.LBRACE, temp)
                    assert nex_lb_pos != -1, "no opening brace for else or if"
                    if tj == T.ELSE:
                        nex_balance = self.find_next_true_pos(nex_lb_pos + 1, temp_balance)
                        assert nex_balance != -1, "no closing brace for else"
                        # add the closing paren after next balance 
                        tj = temp[nex_balance]
                        j = nex_balance
                        if temp[nex_balance] == T.ELSE:
                            # keep looping if the next balanced token is else
                            continue
                        temp.insert(nex_balance + 1, T.RPAREN)
                    else: 
                        temp.insert(nex_lb_pos, T.RPAREN)
                    break
                break
            ts[i+2:] = temp
        self.token_and_strs = ts
        return self.token_and_strs

    def get_detokenized(self, tk_list = None):
        if tk_list is None: tk_list = self.token_and_strs
        T = Preprocess.Tokens
        ret = ""
        for t in tk_list:
            if isinstance(t, T):
                ret += self.token_to_str[t]
            else:
                ret += t
        return ret

def preprocess_if(s : str) -> str:
    p = Preprocess(s)
    p.tokenize()
    p.process_tokenized()
    return p.get_detokenized()

if __name__ == "__main__":
    arg : str = sys.argv[1]
    # arg can either be string or file path
    if arg.endswith(".in"):
        s = ""
        with open(arg, "r") as f:
            s = f.read()
            s = preprocess_if(s)
        # create .out file 
        with open(arg.replace(".in", ".out"), "w") as f:
            print("wrote ", s)
            f.write(s)
    else:
        print(preprocess_if(arg))