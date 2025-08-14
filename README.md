# Aether
oo multithreaded programming language with a focus on speed and portability and compatibility and readability
Aether: High-level feel, System-level power.

![alt text]()

Aether is a modern, object-oriented programming language designed for developers who demand uncompromising performance, intuitive control over memory, and seamless compatibility across platforms. multithreading support.

It combines the readability and high-level abstractions of modern languages with the raw power and low-level control of systems programming. Aether is built on the philosophy that you shouldn't have to sacrifice safety and ease-of-use for speed.

Core Features

Performance: Aether is designed to be as fast as Zig. It achieves this through a transparent compilation process, no hidden control flow, and explicit memory management with powerful compile-time checks.[1][2]

Intuitive Memory Management: Aether introduces "Scoped Arenas," a novel approach to memory allocation that combines the safety of ownership models with the clarity of manual allocation. No garbage collector, no complex lifetime annotations—just clear, predictable memory behavior.

Extreme Platform Compatibility: With a primary focus on Linux and Windows, Aether's compiler is architected for straightforward cross-compilation. Its standard library provides a consistent API for interacting with system resources, from filesystems to hardware devices.[2]

Direct Driver Interaction: Aether is suitable for developing kernel-level drivers.[3][4] It provides features for direct memory access and interaction with hardware registers, all within a type-safe and modern language structure.[5]

Seamless Interoperability: Aether has a first-class Foreign Function Interface (FFI) that makes it trivial to use C libraries and to be embedded within other applications.[6][7]

Quick Look: Hello, Aether!
code
Aether
download
content_copy
expand_less

// main.ae - A simple Aether program

// Import the standard i/o library
import std.io;

// Every program starts with a 'main' function.
// 'pub' makes it accessible to the system loader.
pub func main() -> (void) {
    // Create a new Console object.
    // 'let' declares a constant.
    let console = io.Console.new();

    // Call the 'writeLine' method.
    console.writeLine("Hello from Aether!");
}
The Aether Philosophy & Structure
1. Object-Oriented, but not at the cost of performance

Aether is fundamentally object-oriented. It provides classes, interfaces, and inheritance to structure your code in a clean and maintainable way. However, it avoids performance pitfalls of some traditional OOP languages:

Value-based objects by default: Class instances are stored on the stack unless explicitly allocated on the heap. This avoids unnecessary memory fragmentation and cache misses.

No hidden virtual calls: The compiler will tell you when a method call is dispatched dynamically. You can mark methods as final to ensure static dispatch.

2. Intuitive Memory: Scoped Arenas

Aether simplifies memory management with a system called "Scoped Arenas." Instead of managing individual allocations, you allocate an "arena" for a specific scope or task. All objects created within that scope are placed in the arena. When the scope ends, the entire arena is deallocated at once.

code
Aether
download
content_copy
expand_less
IGNORE_WHEN_COPYING_START
IGNORE_WHEN_COPYING_END
import std.mem;
import std.net;

func handle_request(request: net.Request) {
    // Create a memory arena for this specific request.
    // The arena will automatically be freed when this function exits.
    using arena = mem.Arena.new(1024); // 1KB arena

    // All subsequent allocations using '.new(&arena)' are placed here.
    let headers = http.Headers.new(&arena, from: request);
    let response = process_data(&arena, headers);

    send_response(response);

} // 'arena' and all memory allocated within it is freed here. No 'free()' calls needed.

This approach eliminates the risk of memory leaks for the vast majority of use cases while remaining explicit and highly performant. For long-lived objects, a global arena or manual allocation is still possible.

3. As Fast as Zig: The 'Comptime' Advantage

Inspired by Zig, Aether features a powerful compile-time code execution engine known as comptime.[1][8] This allows you to run Aether code during compilation to generate types, pre-calculate values, and build custom data structures tailored to your specific needs.

code
Aether
download
content_copy
expand_less
IGNORE_WHEN_COPYING_START
IGNORE_WHEN_COPYING_END
// Generic Vector that stores its size at compile time.
class Vec<T: type, comptime N: int> {
    private data: [N]T;

    pub func len() -> (int) {
        // This value is known at compile time.
        return N;
    }
}

// ...

// Create a vector of 3 integers.
// The size is part of the type, leading to significant optimizations.
let my_vec = Vec<int, 3>.new();
4. Unmatched Compatibility and Interaction

Aether is designed to work with the vast ecosystem of existing software and hardware.

Driver Development: Aether can be used to write kernel-level code. It allows for precise control over memory layout and provides built-in types for interacting with hardware ports and registers. The standard library offers support for different system calling conventions.[5]

Foreign Function Interface (FFI): Interacting with C is as simple as importing a header file. The Aether compiler can parse C headers and automatically generate the necessary bindings.

code
Aether
download
content_copy
expand_less
IGNORE_WHEN_COPYING_START
IGNORE_WHEN_COPYING_END
// Import the C standard library's 'printf' function.
importc "stdio.h";

pub func main() -> (void) {
    // Call a C function directly from Aether.
    c.printf("Hello from C, via Aether!\n");
}
Getting Started (Hypothetical)

Download the Compiler:
Head to aether-lang.org/downloads and grab the toolchain for your platform (Windows or Linux).

Compile your first program:

code
Sh
download
content_copy
expand_less
IGNORE_WHEN_COPYING_START
IGNORE_WHEN_COPYING_END
aetherc main.ae -o hello

Run it!

code
Sh
download
content_copy
expand_less
IGNORE_WHEN_COPYING_START
IGNORE_WHEN_COPYING_END
./hello
Join the Vision

Aether is more than just a language; it's a vision for the future of systems programming where performance and developer experience are not competing ideals. We are just getting started and welcome anyone who wants to contribute to the compiler, standard library, or documentation.

License: Aether is distributed under the MIT License.

Sources
help
soufianebouchaara.com
fastly.com
quora.com
quora.com
codeguru.com
wikipedia.org
wikipedia.org
belief-driven-design.com
Google Search Suggestions
Display of Search Suggestions is required when using Grounding with Google Search. Learn more
what makes the Zig programming language fast
intuitive memory allocation strategies in programming languages
how programming languages support driver development
foreign function interface best practices for language interoperability
Aether programming language
