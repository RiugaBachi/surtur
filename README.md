# surtur

[![GitHub CI](https://github.com/Riuga/surtur/workflows/CI/badge.svg)](https://github.com/Riuga/surtur/actions)
[![Build status](https://img.shields.io/travis/Riuga/surtur.svg?logo=travis)](https://travis-ci.org/Riuga/surtur)
[![Hackage](https://img.shields.io/hackage/v/surtur.svg?logo=haskell)](https://hackage.haskell.org/package/surtur)
[![BSD-3-Clause license](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)


> The Northmen say that at Ragnarok, the sons of Muspell will travel to battle in the ship called Naglfar: the corpse ship; and when the sons of Muspell leave the ship and ride to battle, it will be as though the sky had split open, and Surtur will lead them. Wherever he goes, flame will erupt before him, and fires will burn behind him....
> <div style="text-align: right"> - Druth, Hellblade I: Senua's Sacrifice </div>

`surtur` is a high level library that aims to make Vulkan development in Haskell as painless and safe as possible, without detracting from the API's power. To this end, `surtur` introduces monads, arrows, and static types that limit incorrect usage of Vulkan. `surtur` does not try to be overly-restricting---unlike GPipe, it does not dictate how shaders should be written, and provides an escape hatch to ignore type-level constraints on extensions and core versioning. End users are free to write regular GLSL, however they are strongly encouraged to give [fir](https://gitlab.com/sheaf/fir) a try as it more strongly aligns with `surtur`'s strongly-typed philosophy.

# Features

- `Vulkan (u :: Nat) (l :: Nat) e` monad
  - Type level extension list
  - Type level core versioning
- Multi-backend surface initialization
  - `sdl2` and `GLFW-b` supported
  - Compile your project with `surtur:sdl2` or `surtur:GLFW-b` respectively
  - Interface exposed for supporting other backends
- Streamlined device intialization
  - Via custom `rateDevice` function
  - Sane default `rateDevice` included (with MSAA enabled)
- `CommandRecorder` monad
  - `CommandBuffer t s` type where t ∈ {Compute, Graphics, Transfer}, s ∈ {Recording, Executing, Idle}
  - `vkCmd` functions can only be called on the correct command buffer type
  - Buffer queueing protected by `s` parameter
- `RenderPass` graph notation
  - Specify relation between subpasses and external commands symbolically

# Priority Extensions

`surtur` will not support individual AMD or NV extensions. That said, it will support the following KHR and EXT extensions deemed useful:

- TBA

# Compiling

**Library**: `cabal build`


