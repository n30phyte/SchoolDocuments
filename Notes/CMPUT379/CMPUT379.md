# CMPUT 379: Operating System Concepts

## Compiling, Linking, and Loading

When an operating system wants to run a program, it has to **load** it first. Most modern operating systems also have dynamic library loading.

Static compilation and loading everything at the start is not favoured as a program can fail right after execution starts, wasting all that effort.

The program to be loaded has to be compiled and linked beforehand too, since computers can't run source code directly.

### Compilation

Compilation steps:

1. Preprocess: Remove comments, handle includes
2. Compile: Convert source to assembly
3. Assembly: Convert assembly to object code
4. Linking: Combine object code

Basic storage classes for C programs:

```c
// visible in all modules (sections: common, bss)
int some_global_variable;

// global/local: global in current module,
// but not outside (section: data)
static int some_local_variable;

int main(void) {
// In the stack, (section: auto)
int some_stack_variable;
}
```

In C, functions are called with parameters pushed in right to left order into the stack, so

```c
scanf("%d", &res);
```

becomes

```assembly_x86
.LC0:
    .string "%d"
    .text

...

pushl %eax
pushl $.LC0
call scanf
```

### Linking

#### ELF File Format
