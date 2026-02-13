# CIP-30

Cardano dApp-Wallet Web Bridge — a minimal web tool for interacting with any CIP-30 compatible wallet.

**[Live demo](https://paolino.github.io/eternl-tx-signer/)**

## Features

- Detect all installed CIP-30 wallets and connect to any of them
- View wallet info: network, balance, UTXOs, addresses
- Sign transactions (partial signing for multisig)
- Sign arbitrary data (CIP-8)
- Submit fully-signed transactions
- Copy results to clipboard

## Stack

- [PureScript](https://www.purescript.org/) + [Halogen](https://purescript-halogen.github.io/purescript-halogen/)
- [Spago](https://github.com/purescript/spago) build & bundle
- [Nix](https://nixos.org/) development environment
- GitHub Pages deployment

## Development

```bash
nix develop
just build    # compile PureScript
just bundle   # bundle for browser
just serve    # local dev server on :8080
just ci       # lint + build + bundle
```

## Links

- [CIP-30 specification](https://cips.cardano.org/cip/CIP-30)
- [Repository](https://github.com/paolino/eternl-tx-signer)
