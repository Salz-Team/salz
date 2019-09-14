<template>
  <div>
    <navbar />

    <section class="main-content columns">
      <nuxt />
    </section>
  </div>
</template>

<script>
import Navbar from '~/components/Navbar';

export default {
  components: {
    Navbar
  }
};
</script>
