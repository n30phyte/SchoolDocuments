---
id: e8e41156-d701-400a-b6d2-d2abf18bfb17
title: 'CMPUT 379'
desc: 'Operating System Concepts'
updated: 1620708228565
created: 1619146012985
---

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

### Linking and Loading

Compilation produces object files that can be loaded into any physical memory location, a **relocatable object file**.

The linker takes these files and combines them into a single executable, and also handles references to external library files like the libc. For dynamically linked libraries, the references are **stubbed** and get loaded on runtime. A dynamic linker will rewrite the stubs to the correct locations on runtime too.

The loader takes the binary file, and **loads** the file into memory. The loader adjust the memory locations in the code to be correct based on where the program gets loaded in real memory. Dynamically loaded libraries will only get loaded when required, with references resolved by the dynamic linker.

On x86, the convention is for heaps to grow up and stacks to go down (low address is down, high address is up). For 32 bit systems, the last 1 GB is OS, the first 3GB will be user space. The user space area is carved up into smaller pieces that look like the following:
```
0xFFFF FFFF +----------------+
            |  Kernel space  |
0xC000 0000 +----------------+
            |   User stack   |
            |      ...       |
            |                |
            +----------------+
            |                |
            |      ...       |
            |  Library code  |
            +----------------+
            |                |
            |      ...       |
            |      Heap      |
            +----------------+
            | R/W Data (BSS) |
            +----------------+
            | R/W Data (data)|
            +----------------+
            | R/X Data (text)|
0x0000 0000 +----------------+
```

The lowest 3 sections (BSS, data, text) are allocated based on information stored in the executable file. Data generally contains globals and static variables that are initialized, BSS contains uninitialized globals and statics. BSS is usually zero filled by execve.

#### ELF File Format

ELF files start with a file header at `0x00 - 0x04` that reads `0x7f 0x45 0x4c 0x46`, which is `0x7f` followed by ELF in ASCII.
Right after is the bit format of the file, the endianess then the version number which is always `1`.

The file will identify itself as either a relocatable module or an executable, and if it is statically or dynamically linked, along with the library information if dynamic.

### System Calls and Interrupts

System calls are _rarely_ ever used directly by programmers, as we interact with the operating system usually through other libraries that execute the system calls for us (e.g. glibc, rust std lib, C++ stdlib). They still eventually get called however, as you can't exist without the help of of the operating system.

An interrupt service routine is something similar, but generally used for interfacing with hardware. An example is for interaction with I/O devices. External devices can trigger an interrupt to block user processes at any time. When this happens, the operating system takes control to and processes whatever the interrupt wants it to, before returning control to the user.

The operating system abstracts most things that exist physically as a resource for the software. For example, files on the local drive and files on a cloud server. It might not actually exist as a resource but can be treated as such by having the kernel unify the data structures used to represent it. This is also true for the sharing of resources. Trying to obtain a lock on a resource might simply pause execution of your program, and it will continute running after the resource is available as if nothing happened.

#### I/O access example

```
=====================================================================================================
||          ||       IO request                                           P1 complete              ||
||P1        || I========I                                        I============I                    ||
||          ||          |            IO request                  |            |       P2 Complete  ||
||P2        ||          |     I==========I                       |            |     I=======I      ||
||          ||          |     |          |                       |            |     |       |      ||
||Idle      ||          |     |          |     I===========I     |            |     |       I======||
||          ||        p1 blocked         check IO          |     |          switch jobs            ||
||Kernel    ||          I==+==I          I==+==I           I==+==I            I==+==I              ||
||          ||             |                |                 |                  |                 ||
||IO Device ||             I==================================I******************I                 ||
||          ||          start op                         start op 2                                ||
=====================================================================================================
```

### Scheduling

There are 3 classes of activities on a computer system:

* Processes
  * Activities that can be stopped/resumed.
* ISR/Syscalls
  * Activities that handles events for the system.
* Kenel
  * Basically ISRs and Syscalls + small helpers.

A process basically has 3 states:

* Running
* Ready
* Blocked

An expanded list of states would be:

* Running
* Ready
* Blocked
* Suspended Ready
* Suspended Blocked

#### Threads vs Processes

A process is a single execution over the an address space, while a threading is having multiple execution paths in the **same** address space.

A thread has it's own stack and registers at minimum, but generally shared everything else.
Some threading setups have extra "luxuries" such as thread-private data and resources, but they are not required.

Advantage of using threads:

* A multithreaded process can save memory as you don't need to make another copy of the process' memory space to run.
* A blocked thread can swap to a different thread in the same process, instead of the entire process blocking.
* A context switch between threads in a single process is a "lightweight" context switch, only saving registers.

#### Schedulers

FIFO as a scheduling policy is decent, but not the best. The idea of scheduling is to allow the oprating system to give an educated on what process should run next and improve average turnaround time without affecting throughtput.

Two common simple scheduling algorithms are Shortest Jobs First and First Come First Serve (FIFO/FCFS). Both schedulers have the same throughtput, but SJF has the shortest average turnaround time.

Example:
3 Processes: [24, 4, 3]

* Shortest Jobs First (SJF)
  * Complete shortest jobs first: [3, 4, 24]. Average = $\frac{3+7+31}{3} = 13.67$. Throughtput = 3 jobs/31 seconds
* First Come First Serve (FCFS)
  * Complete jobs as they come first: [24, 4, 3]. Average = $\frac{24+28+31}{3} = 27.67$. Throughtput = 3 jobs/31 seconds

There is no perfect scheduler. Different scenarios require different scheduling policies.

* Long-term scheduling: Scheduling decisions are only done once per job, usually done for batch systems.
* Intermediate-term scheduling: Scheduling decisions should only be done once every few seconds.
* Short-term scheduling: This is what CPU scheduling usually is. Process switching happens many times in the process' lifetime.

Shortest Job First gives priority for processes with short CPU bursts (I/O Bound work). To predict how long the CPU burst will last, a simple method used is by taking historical data and claculating an exponential moving average.

$$
  \tau_{n+1} = \alpha t_n + \left(1-\alpha\right)\tau_n
$$

where $t_n$ is the nth sample, $\tau_n$ is the nth average, and $\alpha$ is the recency coeffecient. This will be determined by the algorithm, where as $\alpha \rightarrow 1$, the less the history matters.

##### Preemptive scheduling

The problem with this is that some programs might not swap to I/O fast enough. This means that the scheduler needs to find other ways of making sure that other programs get runtime.

This requires the algorithm to **preempt** the running program. This is usually done by assigning the program an estimation of how long it's CPU burst should be, then swapping it out if it goes over that estimation. If new program is estimated to have a shorter CPU burst, the current program gets preempted too.

Comparison example:

```
P1 ========                 |
P2  **                      |
P3      ############        | Processes
P4             ++++         |
 0 I========================I 26 Timeline
Non-preemptive SJF:
   ========**############++++
 0 I========================I 26 Timeline
Preemptive SJF:
   =**=======##++++##########
 0 I========================I 26 Timeline
```

The throughtput ends up being the same, but the turnaround time is shorter.

Non-preemptive:
$$
  \frac{8+9+17+10}{4} = 12
$$

Preemptive:
$$
  \frac{10+2+21+4}{4} = 9.25
$$

A scheduling algorithm might use priorities instead. A common formula for priority with SJF is:
$$
P = \frac{1}{\tau}
$$
where $\tau$ is the predicted duration of the next CPU burst.

SJF can starve a long burst process if many short burst processes get queued up. This can be mitigated by adding a process age factor.

#### Interactivity

Turnaround time as a metric is great, but when users are involved, response time might be a more important thing to consider. A user starting a program on the terminal would rather have their program run now instead of get queued up to run later due to several shorter jobs that are queued at the moment.

### Synchronization

Synchronization is a requirement when working with more than 1 thread to ensure validity of shared data.

#### Locks and Mutexes

A simple solution is a lock. The POSIX threads library (pthreads) calls it the mutex as it is used for mutual exclusion between threads; when one thread is in a critical section, no other threads can enter theirs.

There are many ways to build a lock/mutex. The following are the generally used ones:

##### Controlling interrupts

By masking interrupts, the OS can ensure that within the critical section, nothing can interrupt it, basically making it atomic. However to do this, we need to allow the program to call elevated instructions, which is a security risk as it can simply mask the interrupt and never let it go since time slicing will stop. This solution also does not work with multicore CPUs as the threads could be running in a different core and simply continue to run. When interrupts get masked, software interrupts are discarded, so we would lose information.

##### Simple Flag

We can have a true/false flag value that's set or cleared based on if a thread is in the section at the moment.

```c
  flag = 0;
  while(flag);
  flag = 1;
  // Critical section
  flag = 0;
```

For example, thread 1 just passed line 2 and a context switch to thread 2 happens, which then brings it just past line 2 too. They both would have went past the barrier, and both would set `flag = 1`. This would cause the critical section to be no longer safe.

08-02 -- 12-02

https://discord.com/channels/402891511991369740/402923930874281985/811094453540487218
