export default ($axios) => (resource) => ({
  index() {
    return $axios.$get(`${resource}`);
  },
  login() {
    return $axios.$get('login');
  }
});
