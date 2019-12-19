export default {
  getFirstFrame: (state) => state.firstFrame,
  getLastFrame: (state) => state.lastFrame,
  getActiveFrame: (state) => state.activeFrame,
  getTurnId: (state) => state.turnid,
  getFramesLength: (state) => state.framesLength,
  getNowPlaying: (state) => state.nowPlaying,
  getHideUI: (state) => state.hideUI,
  getShowHotkeys: (state) => state.showHotkeys,
  getShowRanking: (state) => state.showRanking,
  getShowHelp: (state) => state.showHelp
};
