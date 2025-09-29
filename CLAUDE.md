# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Racket-based music theory and audio manipulation library. It provides utilities for working with musical notes, chords, scales, and Neo-Riemannian transformations. The project also includes audio synthesis capabilities using the `rsound` package.

## Dependencies

- **Racket**: Main language runtime (tested with v8.16)
- **rakeda**: Functional programming library (external dependency)
- **rsound**: Audio synthesis package
- **threading**: For threading macros
- **data/functor**: For functor operations
- **rackunit**: For testing

## Common Commands

### Running Tests
```bash
# Run individual test files
racket core-test.rkt
racket chord-test.rkt
racket scale-test.rkt

# Run all tests with raco
raco test .
```

### Running the REPL
```bash
racket
> (require "music.rkt")
```

### Running Examples
```bash
racket examples/random-tone-example.rkt
racket examples/vibrato-tone.rkt
```

## Architecture

### Module Structure

The codebase is organized into focused modules with clear separation of concerns:

- **music.rkt**: Top-level module that provides a unified interface to all functionality
- **core.rkt**: Foundation module with note manipulation, transposition, and basic utilities
- **chord.rkt**: Chord data structures, conversion functions, and chord generation
- **scale.rkt**: Scale definitions and scale-to-notes conversion
- **neoriemann.rkt**: Neo-Riemannian transformation functions (P, L, R operations)
- **visual.rkt**: Chord visualization on modulo-12 circle
- **audio.rkt**: Audio playback functions (commented out in main module)
- **sound.rkt**: Instrument definitions and sound synthesis

### Core Data Types

- **Notes**: Represented as integers modulo 12 or symbols ('C to 'B)
- **Chords**: Struct with root note and chord name, e.g., `(chord 'E 'minor)`
- **Scales**: Based on root note and scale name (major, minor, etc.)

### Key Design Patterns

- Functions are curried for easy composition in higher-order contexts
- Conversion functions follow naming pattern: `type1->type2`
- Map functions provided for applying transformations: `map-note`, `map-chord`
- Utility functions use functional programming style with `threading` macros

### Module Dependencies

```
music.rkt (top-level)
├── core.rkt (foundational)
├── chord.rkt (depends on core.rkt)
├── scale.rkt (depends on core.rkt)
├── neoriemann.rkt (depends on chord.rkt)
├── visual.rkt (depends on chord.rkt)
├── audio.rkt (depends on sound.rkt, chord.rkt)
└── sound.rkt (audio synthesis)
```

## Development Notes

- Test files follow the pattern `*-test.rkt` and use `rackunit`
- Audio functionality requires `libportaudio.2.dylib` (included)
- Compiled bytecode is stored in `compiled/` directory
- Examples demonstrate practical usage patterns for audio generation