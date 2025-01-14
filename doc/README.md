# Plutus documentation site

This is a sphinx site. You can build it with sphinx directly (assuming you're in a `nix-shell`):

```
$ cd doc
$ sphinx-build -j 4 -n . _build # Open '_build/index.html' in your browser
```

Or you can build it with Nix at the top level, which will also build the Haddock for the project and link it in:

```
$ nix build -f default.nix docs.site
```

The doc site from main is built automatically and hosted [here](https://plutus-apps.readthedocs.io/en/latest).
Additionally, the site is built for all PRs, and a link to a preview can be found in the PR statuses.
