export default $axios => resource => ({
  index: () => $axios.$get(`${resource}`),
  snapshot: turn => $axios.$get(`${resource}${turn ? `/${turn}` : ``}`),

  /**
   * @function moves
   * @param {string} turns Moves API query param
   * @returns {axios} axios instance
   */
  moves: turns => $axios.$get(`${resource}${turns ? `/${turns}` : ``}`),
});
