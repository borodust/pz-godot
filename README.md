# PZ-GODOT

Bindings to [Godot](https://github.com/godotengine/godot) game engine for [Pozzo](https://github.com/borodust/pozzo).

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
And once `libgodot` built:
```common-lisp
(ql:quickload :pz-godot/example)
(godot.example:run)
```
