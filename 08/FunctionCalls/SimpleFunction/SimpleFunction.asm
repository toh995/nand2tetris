// function SimpleFunction.test 2
(SimpleFunction.test)
@SP
D=M
@LCL
M=D
A=D
M=0
D=D+1
A=D
M=0
D=D+1
@SP
M=D
// push local 0
@0
D=A
@LCL
A=D+M
D=M
@SP
M=M+1
A=M-1
M=D
// push local 1
@1
D=A
@LCL
A=D+M
D=M
@SP
M=M+1
A=M-1
M=D
// add
@SP
M=M-1
A=M
D=M
@SP
A=M-1
M=M+D
// not
@SP
A=M-1
M=!M
// push argument 0
@0
D=A
@ARG
A=D+M
D=M
@SP
M=M+1
A=M-1
M=D
// add
@SP
M=M-1
A=M
D=M
@SP
A=M-1
M=M+D
// push argument 1
@1
D=A
@ARG
A=D+M
D=M
@SP
M=M+1
A=M-1
M=D
// sub
@SP
M=M-1
A=M
D=M
@SP
A=M-1
M=M-D
// return
// Store the return address for later usage
@LCL
D=M
@5
D=D-A
A=D
D=M
@R13
M=D
// Copy the return value onto argument 0
@SP
A=M-1
D=M
@ARG
A=M
M=D
// Reset @SP
D=A+1
@SP
M=D
//Restore the caller's segment pointers
@LCL
D=M
@R14
M=D
@R14
M=M-1
A=M
D=M
@THAT
M=D
@R14
M=M-1
A=M
D=M
@THIS
M=D
@R14
M=M-1
A=M
D=M
@ARG
M=D
@R14
M=M-1
A=M
D=M
@LCL
M=D
// Jump to the return address
@R13
A=M
0;JMP