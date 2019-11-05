export const state = () => ({
  activeFrame: 0,
  framesLength: 0
});

export const mutations = {
  setActiveFrame: (state, index) => {
    state.activeFrame = index;
  },
  setFramesLength: (state, length) => {
    state.framesLength = length;
  }
};

export const actions = {
  setActiveFrame({ commit }, index) {
    commit('setActiveFrame', index);
  },
  setFramesLength({ commit }, frames) {
    commit('setFramesLength', frames.length);
  }
};

export const getters = {
  getActiveFrame: (state) => state.activeFrame,
  getFramesLength: (state) => state.framesLength
};
