@256
D=A
@SP
M=D
// call Sys.init 0
// Save the current stack frame
@Sys.init_RETURN4
D=A
@SP
M=M+1
A=M-1
M=D
@LCL
D=M
@SP
M=M+1
A=M-1
M=D
@ARG
D=M
@SP
M=M+1
A=M-1
M=D
@THIS
D=M
@SP
M=M+1
A=M-1
M=D
@THAT
D=M
@SP
M=M+1
A=M-1
M=D
// Reset @ARG
@SP
D=M
@5
D=D-A
@ARG
M=D
// Manage the program jumps
@Sys.init
0;JMP
(Sys.init_RETURN4)
// function Main.fibonacci 0
(Main.fibonacci)
@SP
D=M
@LCL
M=D
@SP
M=D
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
// push constant 2
@2
D=A
@SP
M=M+1
A=M-1
M=D
// lt
@SP
M=M-1
A=M
D=M
@SP
A=M-1
D=M-D
@TRUE0
D;JLT
D=0
@CONTINUE0
0;JMP
(TRUE0)
D=-1
@CONTINUE0
0;JMP
(CONTINUE0)
@SP
A=M-1
M=D
// if-goto N_LT_2
@SP
M=M-1
A=M
D=M
@N_LT_2
D;JNE
// goto N_GE_2
@N_GE_2
0;JMP
// label N_LT_2
(N_LT_2)
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
// label N_GE_2
(N_GE_2)
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
// push constant 2
@2
D=A
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
// call Main.fibonacci 1
// Save the current stack frame
@Main.fibonacci_RETURN1
D=A
@SP
M=M+1
A=M-1
M=D
@LCL
D=M
@SP
M=M+1
A=M-1
M=D
@ARG
D=M
@SP
M=M+1
A=M-1
M=D
@THIS
D=M
@SP
M=M+1
A=M-1
M=D
@THAT
D=M
@SP
M=M+1
A=M-1
M=D
// Reset @ARG
@SP
D=M
@6
D=D-A
@ARG
M=D
// Manage the program jumps
@Main.fibonacci
0;JMP
(Main.fibonacci_RETURN1)
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
// push constant 1
@1
D=A
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
// call Main.fibonacci 1
// Save the current stack frame
@Main.fibonacci_RETURN2
D=A
@SP
M=M+1
A=M-1
M=D
@LCL
D=M
@SP
M=M+1
A=M-1
M=D
@ARG
D=M
@SP
M=M+1
A=M-1
M=D
@THIS
D=M
@SP
M=M+1
A=M-1
M=D
@THAT
D=M
@SP
M=M+1
A=M-1
M=D
// Reset @ARG
@SP
D=M
@6
D=D-A
@ARG
M=D
// Manage the program jumps
@Main.fibonacci
0;JMP
(Main.fibonacci_RETURN2)
// add
@SP
M=M-1
A=M
D=M
@SP
A=M-1
M=M+D
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
// function Sys.init 0
(Sys.init)
@SP
D=M
@LCL
M=D
@SP
M=D
// push constant 4
@4
D=A
@SP
M=M+1
A=M-1
M=D
// call Main.fibonacci 1
// Save the current stack frame
@Main.fibonacci_RETURN3
D=A
@SP
M=M+1
A=M-1
M=D
@LCL
D=M
@SP
M=M+1
A=M-1
M=D
@ARG
D=M
@SP
M=M+1
A=M-1
M=D
@THIS
D=M
@SP
M=M+1
A=M-1
M=D
@THAT
D=M
@SP
M=M+1
A=M-1
M=D
// Reset @ARG
@SP
D=M
@6
D=D-A
@ARG
M=D
// Manage the program jumps
@Main.fibonacci
0;JMP
(Main.fibonacci_RETURN3)
// label END
(END)
// goto END
@END
0;JMP