<template>
  <div class="gameUIContainer frame-control">
    <button @click.prevent.stop="backToInitialFrame">
      <b-icon icon="skip-backward" />
    </button>
    <button @click.prevent.stop="backwardFrame">
      <b-icon icon="step-backward" />
    </button>
    <button @click="togglePlayPause">
      <b-icon v-if="isPlaying" icon="play" />
      <b-icon v-else icon="pause" />
    </button>
    <button @click.prevent.stop="forwardFrame">
      <b-icon icon="step-forward" />
    </button>
    <button class="speed-button" @click.prevent.stop="toggleSpeed">
      {{ speed }}x
    </button>
    <div class="frame-progress-container">
      <input
        id="frame-progress"
        name="frame-progress"
        :title="currentFrameNumber"
        type="range"
        min="0"
        :max="lastFrame"
        :value="currentFrameNumber"
        @input="setFrame"
      />
    </div>
  </div>
</template>

<style lang="scss" scoped>
@import '~assets/css/colors';

$bar-height: 20px;

.frame-control {
  position: fixed;
  bottom: 20px;
  right: 20px;
  width: 50vw;
  min-width: 200px;
  max-width: 500px;
  padding: 10px;
  display: grid;
  grid-template-columns: 20% 20% 20% 20% 20%;
  grid-row-gap: 10px;
  z-index: 1000;
  transition: all 0.2s ease-in-out;

  & > div {
    text-align: center;
  }
}

.frame-progress-container {
  width: 90%;
  height: $bar-height;
  background: $gray;
  grid-column-start: 1;
  grid-column-end: end;
  display: flex;
  justify-content: space-between;
  margin: auto;

  #frame-progress {
    background: $primary;
    height: $bar-height;
    position: relative;
    width: 100%;

    &::-moz-range-thumb,
    &::-webkit-slider-thumb {
      background: $lighter-gray;
      border: 0;
      border-radius: 0;
      height: $bar-height;
      width: 10px;
      cursor: pointer;
      position: absolute;
      right: -10px;
    }
  }
}

@media screen and (max-width: 400px) {
  .frame-control {
    min-width: 0;
    max-width: 100%;
    width: 100%;
    right: 0;
    bottom: 0;
  }
}
</style>

<script charset="utf-8">
import { mapState } from 'vuex';
import { EventBus } from '../../lib/eventBus';

export default {
  name: 'FrameControl',
  data() {
    return {
      speed: 1,
      nowPlayingClock: null,
      progress: 0
    };
  },
  computed: {
    ...mapState({
      currentFrameNumber: (state) => state.game.activeFrame,
      lastFrame: (state) => state.game.lastFrame,
      isPlaying: (state) => state.game.nowPlaying
    })
  },
  methods: {
    togglePlayPause() {
      this.$store.dispatch('game/setNowPlaying', !this.isPlaying);
      if (this.isPlaying) {
        this.setPlay(1000 / this.speed);
      } else {
        clearInterval(this.nowPlayingClock);
        this.nowPlayingClock = null;
      }
      EventBus.$emit('toggledNowPlaying');
    },
    setPlay(dt) {
      this.nowPlayingClock = setInterval(() => {
        if (this.currentFrameNumber < this.lastFrame) {
          this.$store.dispatch('game/goToNextFrame');
        } else {
          this.$store.dispatch('game/setNowPlaying', false);
          clearInterval(this.nowPlayingClock);
          this.nowPlayingClock = null;
        }
      }, dt);
    },
    toggleSpeed() {
      switch (this.speed) {
        case 1:
          this.speed = 2;
          break;
        case 2:
          this.speed = 4;
          break;
        case 4:
          this.speed = 10;
          break;
        case 10:
          this.speed = 30;
          break;
        case 30:
          this.speed = 100;
          break;
        default:
          this.speed = 1;
      }
      if (this.isPlaying) {
        clearInterval(this.nowPlayingClock);
        this.setPlay(1000 / this.speed);
      }
    },
    forwardFrame() {
      this.$store.dispatch('game/goToNextFrame');
    },
    backwardFrame() {
      this.$store.dispatch('game/goToPrevFrame');
    },
    backToInitialFrame() {
      this.$store.dispatch('game/setActiveFrame', 0);
    },
    setFrame() {
      const index = document.querySelector('#frame-progress').value;
      this.$store.dispatch('game/setActiveFrame', index);
    }
  }
};
</script>
