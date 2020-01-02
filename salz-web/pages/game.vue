<template id="salz-game-view">
  <div id="salz-game-inner-view">
    <GameMenu v-if="!hideUI" />
    <FrameControl v-if="!hideUI" />
    <Hotkeys :viewport="viewport" />
    <Ranking v-if="showRanking" />
    <Help v-if="showHelp" />
  </div>
</template>

<style lang="scss">
@import '~assets/css/colors';
#salz-game-inner-view {
  width: 100vw;
  height: 100vh;
  top: 0;
}

.cellInfoContainer {
  padding: 20px;
  position: fixed;
}
</style>

<script charset="utf-8">
import { mapState } from 'vuex';
import { Application } from 'pixi.js';
import { Viewport } from 'pixi-viewport';

// Game Rendering
import GameFrame from '~/lib/game/rendering/gameFrame';
import Frame from '~/lib/game/rendering/frame';
import { Color } from '~/lib/game/colors.js';
import { sketchyMedoid } from '~/lib/game/mathstuff';
import { EventBus } from '~/lib/eventBus';

import GameMenu from '~/components/Game/GameMenu';
import FrameControl from '~/components/Game/FrameControl';
import Hotkeys from '~/components/Game/Hotkeys';
import Ranking from '~/components/Game/Ranking';
import Help from '~/components/Game/Help';

export default {
  components: {
    GameMenu,
    FrameControl,
    Hotkeys,
    Ranking,
    Help
  },
  data() {
    return {
      index: [],
      viewport: null
    };
  },
  computed: {
    ...mapState({
      user: (state) => state.login.username,
      userid: (state) => state.login.id,
      currentFrameNumber: (state) => state.game.activeFrame,
      totalFrames: (state) => state.game.framesLength,
      hideUI: (state) => state.game.hideUI,
      showRanking: (state) => state.game.showRanking,
      showHelp: (state) => state.game.showHelp
    })
  },
  asyncData({ $axios }) {
    return $axios
      .$get('/frames')
      .then((res) => {
        return { index: res };
      })
      .catch((e) => {
        // eslint-disable-next-line
        console.warn(e);
        return { index: { frames: [] } };
      });
  },
  beforeMount() {
    this.$store.dispatch('login/grabToken');
  },
  async mounted() {
    const wrapper = document.querySelector('#salz-game-inner-view');

    // HACK -- This is a temporary solution
    // set a random color for each player
    const cellcolors = [
      Color.pink,
      Color.red,
      Color.orange,
      Color.yellow,
      Color.green,
      Color.turquiose,
      Color.cyan,
      Color.blue,
      Color.purple
    ];
    const playerColorDict = {};
    this.index.frames.forEach((frame) => {
      frame.forEach((player) => {
        const id = player.playerid;
        if (typeof playerColorDict[id] === 'undefined') {
          playerColorDict[id] =
            cellcolors[Math.floor(Math.random() * cellcolors.length)];
        }
        player.color = playerColorDict[id];
      });
    });

    this.$store.dispatch('game/setFramesLength', this.index.frames.length);
    this.$store.dispatch('game/setFirstFrame', 0);
    this.$store.dispatch('game/setActiveFrame', 0);
    if (this.index.frames.length > 0) {
      this.$store.dispatch('game/setLastFrame', this.index.frames.length - 1);
      this.$store.dispatch('game/setTurnId', this.index.frames[0][0].turnid);
    } else {
      this.$store.dispatch('game/setLastFrame', 0);
      this.$store.dispatch('game/setTurnId', 0);
    }

    const frames = [];
    // make first n frames first
    // adjust for lower n for better performance,
    // but possibly at the cost of exposing incomplete preparation
    const criticalFrames = Math.min(10, this.index.frames.length);
    if (this.index.frames.length > 0) {
      for (let i = 0; i < criticalFrames; i++) {
        frames.push(new Frame(this.index.frames[i]));
      }
    }

    // figure out the dimensions for the canvas
    const appWidth = Math.max(
      document.documentElement.clientWidth,
      window.innerWidth || 0
    );
    const appHeight = Math.max(
      document.documentElement.clientHeight,
      window.innerHeight || 0
    );

    const app = new Application({
      width: appWidth,
      height: appHeight,
      backgroundColor: Color.black,
      resizeTo: wrapper,
      resolution: window.devicePixelRatio || 1
    });
    wrapper.appendChild(app.view);

    const vpWorldWidth = 1000;
    const vpWorldHeight = 1000;

    this.viewport = new Viewport({
      screenWidth: window.innerWidth,
      screenHeight: window.innerHeight,
      worldWidth: vpWorldWidth,
      worldHeight: vpWorldHeight,
      interaction: app.renderer.plugins.interaction
    });

    // Set the centre to be the mediod of player's cells
    // const user = JSON.parse(sessionStorage.getItem('user'));
    const playerID = parseInt(this.userid, 10);

    if (playerID == null || this.index.frames.length === 0) {
      this.viewport.moveCenter(0, 0);
    } else {
      // camera should point at middle of cluster.
      this.viewport.moveCenter(
        ...sketchyMedoid(playerID, this.index.frames[0])
      );
    }

    app.stage.addChild(this.viewport);
    this.viewport
      .drag()
      .pinch()
      .wheel()
      .decelerate()
      .clampZoom({
        minWidth: vpWorldWidth / 5,
        minHeight: vpWorldHeight / 5,
        maxWidth: vpWorldWidth * 5,
        maxHeight: vpWorldHeight * 5
      });

    const gameFrame = await new GameFrame(
      this.index.frames.length > 0
        ? frames[0]
        : new Frame([
            {
              turnid: 0,
              playerid: 0,
              pos: []
            }
          ]),
      vpWorldWidth,
      vpWorldHeight
    );
    this.viewport.addChild(gameFrame);

    // continue making the rest of the frames here
    if (this.index.frames.length > 0) {
      for (let i = criticalFrames; i < this.index.frames.length; i++) {
        frames.push(new Frame(this.index.frames[i]));
      }
    }

    // Add a helper function for viewport
    /**
     * Move viewport camera by dx and dy
     *
     * @param   {number}  dx  Pixels to move x by
     * @param   {number}  dy  Pixels to move y by
     */
    this.viewport.moveViewport = function(dx, dy) {
      this.viewport.moveCenter(
        this.viewport.center.x + dx,
        this.viewport.center.y + dy
      );
    };

    EventBus.$on('updateFrameIndex', () => {
      gameFrame.mountFrame(frames[this.currentFrameNumber]);
    });
  }
};
</script>
