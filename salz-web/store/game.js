export const state = () => ({
  activeFrame: 0,
  framesLength: 0,
  nowPlaying: false,
  hideUI: false,
  menuIsActive: false,
  showHotkeys: false,
  showRanking: false,
  showHelp: false
});

export const mutations = {
  setActiveFrame: (state, index) => {
    state.activeFrame = index;
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
  }
};

export const actions = {
  setActiveFrame({ commit }, index) {
    commit('setActiveFrame', index);
  },
  setFramesLength({ commit }, frames) {
    commit('setFramesLength', frames.length);
  },
  setNowPlaying({ commit }, tf) {
    commit('setNowPlaying', tf);
  },
  setMenuIsActive({ commit }, tf) {
    commit('setMenuIsActive', tf);
  },
  setShowHotkeys({ commit }, tf) {
    commit('setShowHotkeys', tf);
    if (tf) {
      commit('setMenuIsActive', false);
      commit('setShowRanking', false);
      commit('setShowHelp', false);
    }
  },
  setShowRanking({ commit }, tf) {
    commit('setShowRanking', tf);
    if (tf) {
      commit('setMenuIsActive', false);
      commit('setShowHotkeys', false);
      commit('setShowHelp', false);
    }
  },
  setShowHelp({ commit }, tf) {
    commit('setShowHelp', tf);
    if (tf) {
      commit('setMenuIsActive', false);
      commit('setShowHotkeys', false);
      commit('setShowRanking', false);
    }
  },
  hideUI({ commit }) {
    commit('setHiddenUI', true);
    commit('setMenuIsActive', false);
    commit('setShowHotkeys', false);
    commit('setShowRanking', false);
    commit('setShowHelp', false);
  },
  showUI({ commit }) {
    commit('setHiddenUI', false);
  }
};

export const getters = {
  getActiveFrame: (state) => state.activeFrame,
  getFramesLength: (state) => state.framesLength,
  getNowPlaying: (state) => state.nowPlaying,
  getHideUI: (state) => state.hideUI,
  getShowHotkeys: (state) => state.showHotkeys,
  getShowRanking: (state) => state.showRanking,
  getShowHelp: (state) => state.showHelp
};
