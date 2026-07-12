# PZ-GODOT

Bindings to [Godot 4.7.1](https://github.com/godotengine/godot) game engine for [Pozzo](https://github.com/borodust/pozzo).

## Build Godot

#### Linux
```sh
cd src/lib/

# Godot Editor
./build.sh desktop-editor

# libgodot
./build.sh desktop-library
```

## Example
Link `pz-godot` into quicklisp's local-projects.
And once `libgodot` has been built:

```common-lisp
(ql:quickload :pz-godot/example)
(godot.example:run)
```

This should put Godot's new project wizard onto the screen.
It's not going to be of much use though, because of how Godot 4.6 editor works:
it requires proper integration with Lisp environment, which is fairly involved
and what [Pozzo](https://github.com/borodust/pozzo) is for.
