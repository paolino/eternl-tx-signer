export const detectEternl = () =>
  !!(window.cardano && window.cardano.eternl);

export const enableWalletImpl = (onError) => (onSuccess) => () => {
  window.cardano.eternl.enable()
    .then((api) => onSuccess(api)())
    .catch((err) => onError(new Error(String(err)))());
};

export const signTxImpl = (api) => (cborHex) => (onError) => (onSuccess) => () => {
  api.signTx(cborHex, true)
    .then((witness) => onSuccess(witness)())
    .catch((err) => onError(new Error(String(err)))());
};
