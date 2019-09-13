<template>
  <div>
    <navbar />

    <section class="main-content columns">
      <div class="container column is-12">
        <nuxt />
      </div>
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
