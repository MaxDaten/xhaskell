final: prev: {
  haskell =
    prev.haskell
    // {
      packageOverrides = hself: hsuper:
        prev.haskell.packageOverrides hself hsuper
        // {
          xhaskell = hself.callCabal2nix "xhaskell" ./. { };
          servant-lucid2 = hself.callCabal2nix "servant-lucid2"
            (builtins.fetchGit {
              url = "https://github.com/Briends/servant-lucid2.git";
              rev = "ba035126b063988a7779d13650f180501a9e43d0";
            })
            { };
          lucid2-hyperscript = hself.callCabal2nix "lucid2-hyperscript"
            (builtins.fetchGit {
              url = "https://github.com/Briends/lucid2-hyperscript.git";
              rev = "8d37673c3b10c4163db24626c94ed24dff5f0905";
            })
            { };
          # currently ghc96 is broken with ormolu, so we have to stay with current ghc, but have to override servant packages
          servant = hself.servant_0_20;
          servant-server = hself.servant-server_0_20;
          servant-client = hself.servant-client_0_20;
          servant-client-core = hself.servant-client-core_0_20;
          servant-swagger = hself.servant-swagger_1_2;
          servant-conduit = hself.servant-conduit_0_16;
          lucid2-htmx = prev.haskell.lib.unmarkBroken (prev.haskell.lib.doJailbreak hsuper.lucid2-htmx);
        };
    };
  xhaskell = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.xhaskell;
}
