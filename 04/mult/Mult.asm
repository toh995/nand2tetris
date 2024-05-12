// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/4/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)
// The algorithm is based on repetitive addition.

// R2 = 0
// for (i = R1; i > 0; i--):
//    R2 = R2 + R0

// R2 = 0
@R2
M=0

// i = R1
@R1
D=M
@i
M=D

// Start the loop
@LOOP
0; JMP

(LOOP)
  // if i <= 0, then goto END
  @i
  D=M
  @END
  D; JLE

  // R2 = R2 + R0
  @R0
  D=M
  @R2
  M=D+M

  // i--
  @i
  M=M-1

  // Restart the loop
  @LOOP
  0; JMP

// Infinite loop
(END)
  @END
  0; JMP
