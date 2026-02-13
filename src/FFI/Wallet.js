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

export const getNetworkIdImpl = (api) => (onError) => (onSuccess) => () => {
  api.getNetworkId()
    .then((id) => onSuccess(id)())
    .catch((err) => onError(new Error(String(err)))());
};

export const getBalanceImpl = (api) => (onError) => (onSuccess) => () => {
  api.getBalance()
    .then((bal) => onSuccess(bal)())
    .catch((err) => onError(new Error(String(err)))());
};

export const getUtxosImpl = (api) => (onError) => (onSuccess) => () => {
  api.getUtxos()
    .then((utxos) => onSuccess(utxos || [])())
    .catch((err) => onError(new Error(String(err)))());
};

export const getUsedAddressesImpl = (api) => (onError) => (onSuccess) => () => {
  api.getUsedAddresses()
    .then((addrs) => onSuccess(addrs || [])())
    .catch((err) => onError(new Error(String(err)))());
};

export const getUnusedAddressesImpl = (api) => (onError) => (onSuccess) => () => {
  api.getUnusedAddresses()
    .then((addrs) => onSuccess(addrs || [])())
    .catch((err) => onError(new Error(String(err)))());
};

export const getChangeAddressImpl = (api) => (onError) => (onSuccess) => () => {
  api.getChangeAddress()
    .then((addr) => onSuccess(addr)())
    .catch((err) => onError(new Error(String(err)))());
};

export const getRewardAddressesImpl = (api) => (onError) => (onSuccess) => () => {
  api.getRewardAddresses()
    .then((addrs) => onSuccess(addrs || [])())
    .catch((err) => onError(new Error(String(err)))());
};

export const signDataImpl = (api) => (addr) => (payload) => (onError) => (onSuccess) => () => {
  api.signData(addr, payload)
    .then((sig) => onSuccess(JSON.stringify(sig))())
    .catch((err) => onError(new Error(String(err)))());
};

export const submitTxImpl = (api) => (cborHex) => (onError) => (onSuccess) => () => {
  api.submitTx(cborHex)
    .then((hash) => onSuccess(hash)())
    .catch((err) => onError(new Error(String(err)))());
};
