import { EventBus } from '../../lib/eventBus';

export default {
  setFirstFrame: (state, n) => {
    state.firstFrame = n;
  },
  setLastFrame: (state, n) => {
    state.lastFrame = n;
  },
  setActiveFrame: (state, index) => {
    state.activeFrame = index;
    EventBus.$emit('updateFrameIndex');
  },
  setTurnId: (state, id) => {
    state.turnid = id;
  },
  setFramesLength: (state, length) => {
    state.framesLength = length;
  },
  setNowPlaying: (state, b) => {
    state.nowPlaying = b;
  },
  setHiddenUI: (state, b) => {
    state.hideUI = b;
  },
  setMenuIsActive: (state, b) => {
    state.menuIsActive = b;
  },
  setShowHotkeys: (state, b) => {
    state.showHotkeys = b;
  },
  setShowRanking: (state, b) => {
    state.showRanking = b;
  },
  setShowHelp: (state, b) => {
    state.showHelp = b;
  },
};
