self.onmessage = (e) => {
  postMessage({ status: "finished", message: e.data.gameHistory });
};
