import { parseJWT } from '../lib/jwt';

export const state = () => ({
  status: 'logged out',
  isLoggedIn: false,
  token: null,
  username: null,
  id: null,
});

export const mutations = {
  loginRequest: state => {
    state.status = 'logging in';
  },
  loginSuccess: (state, value) => {
    state.status = 'logged in';
    state.isLoggedIn = true;
    state.token = value;

    const p = parseJWT(value);
    state.username = p.login;
    state.id = p.id;
  },
  loginFailure: state => {
    state.status = 'login failed';
    state.isLoggedIn = false;
    state.token = null;
    state.username = null;
    state.id = null;
  },
  logoutRequest: state => {
    state.status = 'logging out';
  },
  logoutSuccess: state => {
    state.status = 'logged out';
    state.isLoggedIn = false;
    state.token = null;
    state.username = null;
    state.id = null;
  },
};

export const actions = {
  updateToken({ commit }, jwt) {
    commit('loginRequest');
    if (jwt !== null) {
      localStorage.setItem('auth_token', jwt);
      this.$axios.setToken(jwt, 'Bearer');
      const parsedJwt = parseJWT(jwt);
      const user = {
        username: parsedJwt.login,
        id: parsedJwt.id,
      };
      sessionStorage.setItem('user', JSON.stringify(user));
      commit('loginSuccess', jwt);
    } else {
      commit('loginFailure');
    }
  },
  grabToken({ commit }) {
    const jwt = localStorage.getItem('auth_token');
    if (jwt !== null) {
      this.$axios.setToken(jwt, 'Bearer');

      // check if user info has been stored in session
      const parsedJwt = parseJWT(jwt);
      const storedUser = JSON.parse(sessionStorage.getItem('user'));
      if (storedUser === null) {
        const user = {
          username: parsedJwt.login,
          id: parsedJwt.id,
        };
        sessionStorage.setItem('user', JSON.stringify(user));
      }

      commit('loginSuccess', jwt);
    } else {
      commit('loginFailure');
    }
  },
  dropToken({ commit }) {
    commit('logoutRequest');
    localStorage.removeItem('auth_token');
    sessionStorage.removeItem('user');
    this.$axios.setHeader('Authorization:Bearer', null);
    commit('logoutSuccess');
  },
};

export const getters = {
  token: state => state.token,
  username: state => state.username,
  id: state => state.id,
  isLoggedIn: state => state.isLoggedIn,
};
