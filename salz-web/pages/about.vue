<template>
  <div class="container column is-12">
    <section class="section">
      <div class="is-mobile" v-html="about"></div>
    </section>
  </div>
</template>

<script>
import about from '../md-files/about.md';

export default {
  computed: {
    about() {
      return about;
    }
  }
};
</script>
