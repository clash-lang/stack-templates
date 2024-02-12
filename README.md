# Stack Templates
Templates for `stack new` command. To use this template perform the following steps:
1. [Install Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
2. Run `stack new my-clash-project clash-lang/simple`. Replace `simple` by the template you'd like to use.
3. Read `my-clash-project/README.md`. Enjoy!

## Cabal users
All starter projects are also available on [clash-lang/clash-starters](https://github.com/clash-lang/clash-starters).

## Contributing
If you wish to contribute to this template, edit them in `projects/` and perform the following steps to test the template:
1. Edit the template
2. Run `./render.hs` to instantiate them.
3. Go to the parent directory: `cd ..` and instantiate the template using the rendered `.hsfiles`.
```
cd ..
stack new my-template stack-templates/simple.hsfiles
```
4. Use the template:
```
cd my-template
cabal build
cabal test
```

## License
The default license for each of the starter project is BSD2. However, this whole repository -including every starter project- is licensed under CC0. That means the authors, to the extent possible under law, have waived all copyright and related or neighboring rights to this "Clash Example Project". Feel free to choose any license for the starter projects that you want.
