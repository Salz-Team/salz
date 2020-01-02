<template>
  <div>
    <Navbar />

    <section class="main-content">
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
