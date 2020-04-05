<template>
  <div class="container column is-12">
    <section class="section">
      <div v-html="about" class="is-mobile"></div>
    </section>
  </div>
</template>

<script>
import about from '../md-files/about.md';

export default {
  computed: {
    about() {
      return about;
    },
  },
};
</script>
