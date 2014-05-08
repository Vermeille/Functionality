Functionality
=============

Since I am a really bad developper when it comes to low level stuff, please
consider this work as heavily unstable, and as a playground for me. Trusting
this project is more or less like trusting a total stranger asking to borrow
your credit card for few days, even if he is claiming not to use it for his own
benefit. You got me: don't fucking use that code, or the universe may collapse
because it's too unstable.

Okay, now we can talk about the technical things in this project.

A virtual machine
-----------------

Functionality is a virtual machine which is based on an outrageous plagiarism
of the .Net CLI/CLR design but in a functional paradigm rather than an object
paradigm.

Like all modern and high level asm, Functionality's is thought to be completely
implementation agnostic, so that I can first implement a bytecode interpreter
using "high level"  datastructures (dudes, we're talking 'bout C, no shit, how
could I really say "high level"?), in plain ISO C, machine agnostic. Yes, for
now, we have an asm which knows nothing about the vm, and a vm which never
cares about the hardware (Next step, make all these people meet and make a
social vm, damn, they have feelings too!).

You have only two things to know:

1. The VM is fully Stack based;
2. The VM only handles ints of various sizes and references as primitive types;
3. (Which is induced by 2. so it doesn't really counts) the asm is typed, so be
   careful when using the asm about the types' values on the top, you wouldn't
   do a typecheck error in assembly (lol, ashaming)

You can find full documentation of the opcodes in doc/opcodes.markdown

I tried to be smart (it tooks me a long time, seriously), so, the opcodes
should be really trivial to use in the not-soon-to-come expected JITter.

An assembler
------------

If my english is not completely fucked up, an assembler assembles assembly.

The assembler is still to be written, but it will accept a quite cool grammar,
still plagiarized from CIL's one (because they seriously did a good job,
because I technically suck, and because I have no creativity).

Please dear author, complete this part when you'll have more insights about the
assembler.

A high-level language
---------------------

Coding in asm sucks. Even if it's a high level one. So, I intend to create a
high level language which would be a broken-by-design Haskell (so, similar to
OCaml). If I do my job correctly, there should be a way to embed inline asm and
inline native asm in it.

Oh, yeah, forgot to told ya, I don't have any fucking idea about the executable
format design (PE / Elf are way too mainstream).

The compiler will be probably written in Haskell, and I will avoid write it in
the target language just to make it a little less buggy.

A kernel VM
-----------

If you look at the code (there are exactly zero lines of code at the moment,
just use your imagination), you'll maybe notice that the vm uses no libc at
all. Or functions are redefined. That's because I want that thing to run in
kernel land.

How cool is that? You can now (please consider "now" as a time frame about a
decade) boot on a vm so that you can run absolutely zero programs! Please
reboot on your usual OS immediately and continue scrolling Facebook like
nothing didn't happen.

Then, if my life still sucks at that time, I'll try to write a kernel/OS based
on that.

Conclusion
----------

Let's do a reminder about probabilities: when you stack uncertainties, they
multiply. Let's consider every step of this project is 20% bloated or
broken-by-design, and analyze the stack:

    element     accumulated probability of a correct execution
    nothing     1.00
    VM          0.80
    JITter      0.64
    ASM         0.512
    language    0.4096
    kernel      0.32768
    user app    0.262144

This is why the warning given in preamble should be taken seriously. Or maybe
you can just execute your main 5 times to make it actually run once,
statistically.

