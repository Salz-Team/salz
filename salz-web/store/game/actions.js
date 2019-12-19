export default {
  setFirstFrame({ commit }, n) {
    commit('setFirstFrame', n);
  },
  setLastFrame({ commit }, n) {
    commit('setLastFrame', n);
  },
  setActiveFrame({ state, commit }, index) {
    if (state.firstFrame <= index && state.lastFrame >= index) {
      commit('setActiveFrame', index);
    }
  },
  goToNextFrame({ state, commit }) {
    if (state.activeFrame < state.lastFrame) {
      commit('setActiveFrame', state.activeFrame + 1);
    }
  },
  goToPrevFrame({ state, commit }) {
    if (state.activeFrame > state.firstFrame) {
      commit('setActiveFrame', state.activeFrame - 1);
    }
  },
  setTurnId({ commit }, id) {
    commit('setTurnId', id);
  },
  setFramesLength({ commit }, length) {
    commit('setFramesLength', length);
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
