-- |
-- Module      : Compiler.Ast.Lisp
-- Description : Types to represent the grammar of our output language
-- Copyright   : (c) Functor Factory
-- License     : GPL-3.0 or later
--
-- Maintainer  : jshilling@functorfactory.com
-- Stability   : experimental
-- Portability : portable
--
-- This is the syntax tree for the x86 asm that we generate.
--
-- reg   ::= rsp | rbp | rax | rbx | rcx | rdx | rsi | rdi |
--           r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15
-- arg   ::= $int | %reg | int(%reg)
-- instr ::= addq arg,arg | subq arg,arg | negq arg | movq arg,arg |
--           pushq arg | popq arg | callq labal | retq | jmp label |
--           label: instr
--
-- program ::= .globl main
--             main: instr
module Compiler.Ast.Asm
  ( Reg (..),
    Arg (..),
    Instr (..),
    Block (..),
    Program (..),
  )
where

import Data.List (intercalate)

-- | Represents the set of Registers in the x86_64 arch
data Reg
  = RSP
  | RBP
  | RAX
  | RBX
  | RCX
  | RDX
  | RSI
  | RDI
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15

-- | A possible operand for an instruction
data Arg
  -- | An immediate value (e.g. $42)
  = Imm Int
  -- | A register
  | Reg Reg
  -- | A register to dereference and an offset (e.g. Deref RBP (-8) = -8(%rbp))
  | Deref Reg Int

-- | The set of instructions that might be generated
data Instr
  -- | Add arg1 to arg2 updating arg2 in place
  = ADDQ Arg Arg
  -- | Subtract arg1 from arg2 updating arg2 in place
  | SUBQ Arg Arg
  -- | Multiply arg by -1 updating it in place
  | NEGQ Arg
  -- | Move the value in arg1 to the location arg2
  | MOVQ Arg Arg
  -- | Push the value in arg onto the stack
  | PUSHQ Arg
  -- | Pop the top of the stack into the location at Arg
  | POPQ Arg
  -- | Push a call stack and jump to the label
  | CALLQ String
  -- | Pop a call stack and return to the address referenced in the call frame
  | RETQ
  -- | Jump to a label
  | JMP String

-- | A collection of instructions
newtype Block = Block [Instr]

-- | A collection of blocks, each associated with a label
newtype Program = Program [(String, Block)]

instance Show Reg where
  show RSP = "rsp"
  show RBP = "rbp"
  show RAX = "rax"
  show RBX = "rbx"
  show RCX = "rcx"
  show RDX = "rdx"
  show RSI = "rsi"
  show RDI = "rdi"
  show R8 = "r8"
  show R9 = "r9"
  show R10 = "r10"
  show R11 = "r11"
  show R12 = "r12"
  show R13 = "r13"
  show R14 = "r14"
  show R15 = "r15"

instance Show Arg where
  show (Imm a) = "$" ++ show a
  show (Deref reg offset) = show offset ++ "(" ++ show reg ++ ")"
  show (Reg a) = "%" ++ show a

instance Show Instr where
  show (ADDQ a b) = "addq   " ++ show a ++ ", " ++ show b
  show (SUBQ a b) = "subq   " ++ show a ++ ", " ++ show b
  show (NEGQ a) = "negq   " ++ show a
  show (MOVQ a b) = "movq   " ++ show a ++ ", " ++ show b
  show (PUSHQ a) = "pushq  " ++ show a
  show (POPQ a) = "popq   " ++ show a
  show (CALLQ a) = "callq  " ++ show a
  show RETQ = "retq"
  show (JMP a) = "jmp    " ++ show a

instance Show Block where
  show (Block l) = intercalate "\n" $ map show l

instance Show Program where
  show (Program l) =
    let toString :: (String, Block) -> String
        toString (label, body) =
          let bodyStrs = map ("       " ++) $ lines $ show body
              label' = label ++ ":"
           in intercalate "\n" (label' : bodyStrs)
     in intercalate "\n" (map toString l)
