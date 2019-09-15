export default ($axios) => (resource) => ({
  frames() {
    return $axios.$get(`${resource}`);
  }
});
