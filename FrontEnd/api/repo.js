export default ($axios) => (resource) => ({
  index() {
    return $axios.$get(`${resource}`);
  }
});
