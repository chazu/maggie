# Maggie Project Example

A minimal multi-file Maggie project demonstrating the module system.

## Structure

```
examples/project/
  maggie.toml          # Project manifest
  src/
    Main.mag           # Entry point (Main.start)
    Greeter.mag        # A helper class
```

## Key Concepts

- **maggie.toml** declares the project name, namespace, source directories, and entry point.
- **namespace:** at the top of each `.mag` file places classes in the `GreetingApp` namespace.
- Classes in the same project are auto-imported, so `Greeter` resolves without an explicit `import:`.
- **classMethod: start** is the entry point invoked via `mag -m GreetingApp::Main.start`.

## Running

From the project directory:

```bash
mag -m GreetingApp::Main.start
```

Or from the repository root:

```bash
cd examples/project && mag -m GreetingApp::Main.start
```

## Expected Output

```
Hello, World!
Good day, World. How do you do?
Hello, Alice!
Hello, Bob!
Hello, Charlie!
```
