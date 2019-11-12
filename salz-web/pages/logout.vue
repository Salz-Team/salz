<template>
  <div class="container column is-12">
    <section class="section">
      Logging out...
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
    setTimeout(() => {
      this.$store.dispatch('login/dropToken');
      this.$router.push({ name: 'index' });
    }, 300);
  }
};
</script>
