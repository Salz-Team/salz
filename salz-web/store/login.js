import { parseJWT } from '../lib/jwt';
import axios from '@/plugins/repo';

export const state = () => ({
  status: 'logged out',
  isLoggedIn: false,
  token: null,
  username: null,
  id: null
});

export const mutations = {
  loginRequest: (state) => {
    state.status = 'logging in';
  },
  loginSuccess: (state, value, username, id) => {
    state.status = 'logged in';
    state.isLoggedIn = true;
    state.token = value;
    state.username = username;
    state.id = id;
  },
  loginFailure: (state) => {
    state.status = 'login failed';
    state.isLoggedIn = false;
    state.token = null;
    state.username = null;
    state.id = null;
  },
  logoutRequest: (state) => {
    state.status = 'logging out';
  },
  logoutSuccess: (state) => {
    state.status = 'logged out';
    state.isLoggedIn = false;
    state.token = null;
    state.username = null;
    state.id = null;
  }
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
        id: parsedJwt.id
      };
      sessionStorage.setItem('user', JSON.stringify(user));
      commit('loginSuccess', jwt, parsedJwt.login, parsedJwt.id);
    } else {
      commit('loginFailure');
    }
  },
  grabToken({ commit }) {
    const jwt = localStorage.getItem('auth_token');
    if (jwt !== null) {
      this.$axios.setToken(jwt, 'Bearer');
      const parsedJwt = parseJWT(jwt);

      // check if user info has been stored in session
      const storedUser = JSON.parse(sessionStorage.getItem('user'));
      if (storedUser === null) {
        const user = {
          username: parsedJwt.login,
          id: parsedJwt.id
        };
        sessionStorage.setItem('user', JSON.stringify(user));
      }

      commit('loginSuccess', jwt, parsedJwt.login, parsedJwt.id);
    } else {
      commit('loginFailure');
    }
  },
  dropToken({ commit }) {
    commit('logoutRequest');
    localStorage.removeItem('auth_token');
    sessionStorage.removeItem('user');
    axios.setHeader('Authorization:Bearer', null);
    commit('logoutSuccess');
  }
};

export const getters = {
  token: (state) => state.token,
  username: (state) => state.username,
  id: (state) => state.id,
  isLoggedIn: (state) => state.isLoggedIn
};
