<template>
  <div class="container column is-12">
    <section class="section">
      Logging in with <b-icon icon="github-circle" /> Github
    </section>
  </div>
</template>

<style scoped>
container {
  display: flex;
  justify-content: space-around;
  align-items: center;
}

section {
  text-align: center;
}
</style>

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

    if (key === 'jwt') {
      this.$store.dispatch('login/updateToken', token);
    }

    setTimeout(() => {
      this.$router.push({ name: 'index' });
    }, 200);
  }
};
</script>
