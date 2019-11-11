<template>
  <div class="gameMenu-container">
    <div class="gameUIContainer gameMenu" :class="{ active: menuIsActive }">
      <div v-for="(item, key) of items" :key="key">
        {{ item.title }}
      </div>
    </div>
    <div
      class="gameUIContainer gameMenu-toggle"
      :class="{ active: menuIsActive }"
      @click="menuIsActive = !menuIsActive"
    >
      <b-icon icon="menu" />
    </div>
  </div>
</template>

<style lang="scss" scoped>
.gameMenu-container {
  position: fixed;
  top: 75px;
  display: flex;
  z-index: 101;
}

.gameMenu {
  display: grid;
  grid-template-columns: 33.33% 33.33% 33.33%;
  width: 30vw;
  min-width: 300px;
  max-width: 600px;
  padding: 30px;
  position: relative;
  margin-left: -30vw;
  text-transform: uppercase;
  opacity: 0;
  transition: all 0.5s ease-in-out;

  &.active {
    margin-left: 0;
    opacity: 1;
  }
}

.gameMenu-toggle {
  cursor: pointer;
  margin-left: 10px;
  padding: 10px;
  position: relative;
  display: flex;
  align-items: center;
  left: 0;
  transition: all 0.5s ease-in-out;

  &.active {
    margin-left: 0;
  }
}

@media screen and (max-width: 1300px) {
  .gameMenu {
    grid-template-columns: 50% 50%;
  }
}

@media screen and (max-width: 600px) {
  .gameMenu {
    grid-template-columns: 100%;
  }
}

@media screen and (max-width: 1000px) {
  .gameMenu {
    margin-left: -300px;
  }
}
</style>

<script charset="utf-8">
export default {
  props: {
    items: {
      type: Array,
      required: true
    }
  },
  data() {
    return {
      menuIsActive: false
    };
  }
};
</script>
