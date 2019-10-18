<template>
  <div class="container column is-12">
    <section class="section">
      Logging in with Github
    </section>
  </div>
</template>

<style scoped></style>

<script charset="utf-8">
export default {
  mounted() {
    let query;
    try {
      query = window.location.href.split('?')[1];
    } catch (e) {
      // eslint-disable-next-line
      console.error(e);
    }

    let key, token;
    try {
      [key, token] = query.split('=');
    } catch (e) {
      // eslint-disable-next-line
      console.error(e);
    }

    // this.$store.dispatch('updateToken', token);

    if (key === 'jwt') {
      window.localStorage.setItem('auth_token', token);
      this.$axios.setHeader('Authorization:Bearer', token);
    }

    window.location.href = process.env.baseurl;
  }
};
</script>
