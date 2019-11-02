<template>
  <div class="container column is-12" @dragover.prevent @drop.stop.prevent>
    <section class="section">
      <div class="is-mobile">
        <h1>
          {{ user.username }} &nbsp;
          <small>(Player ID: {{ user.id }})</small>
        </h1>
        <div class="account-info-table">
          <div class="account-info-table-names">
            <b><b-icon icon="access-point" /> Bot Status</b>
          </div>
          <div></div>
          <div class="account-info-table-names">
            <b><b-icon icon="upload" /> Upload bot</b>
          </div>
          <form
            method="post"
            accept-charset="utf-8"
            @submit.prevent="submitBot"
          >
            <label id="botname-label" for="botname">Bot name</label>
            <input
              v-if="botfile !== null"
              id="bot"
              type="text"
              :value="botfile.name"
              name="bot"
            />
            <input v-else id="bot" type="text" value="" name="bot" />
            <div
              id="drop_zone"
              :class="{ dragover: isDraggedOver }"
              @drop.stop.prevent="dropHandler"
              @dragleave="isDraggedOver = false"
              @dragenter="isDraggedOver = true"
              @dragover.prevent
              @dragend="isDraggedOver = false"
            >
              <label
                v-if="botfile === null"
                id="input-bot-label"
                for="input-bot"
              >
                Click or drop your <code>&lt;botname&gt;.tar.gz</code> file
                here.
              </label>
              <input
                id="input-bot"
                name="input-bot"
                accept=".tar.gz"
                type="file"
                @change="fileInputHandler"
              />
              <div v-if="botfile !== null" class="botfile-container">
                <span><b-icon icon="zip-box" /> {{ botfile.file.name }}</span>
                <button class="bot-remove-button" @click="botfile = null">
                  <b-icon icon="close" />
                </button>
              </div>
            </div>
            <button type="Submit" :disabled="botfile == null">Submit</button>
          </form>
        </div>
      </div>
    </section>
  </div>
</template>

<style lang="scss" scoped>
@import '~assets/css/colors.scss';

.account-info-table {
  display: grid;
  grid-template-columns: 30% 70%;
  grid-row-gap: 50px;

  .account-info-table-names {
    display: flex;
    align-items: center;
    justify-content: space-around;
  }
}

#drop_zone {
  margin: 10px;
  padding: 20px;
  padding-bottom: 100px;
  border: 1px dashed var(--body-gray-color);
  border-radius: 5px;
  text-align: center;
  display: flex;
  justify-content: space-around;
  align-items: center;
  transition: background 0.2s ease-in-out;

  &.dragover {
    background: var(--mask-bg-1);
  }
}

#botname-label,
#bot {
  display: none;
}

#input-bot-label {
  border: 1px solid var(--body-fg-color);
  border-radius: 2px;
  padding: 10px;
  transition: background 0.2s ease-in-out;
  cursor: pointer;

  &:hover {
    background: var(--mask-bg-1);
  }
}

#input-bot {
  display: none;
}

.botfile-container {
  width: 100%;
  margin-top: 15px;
  display: flex;
  justify-content: space-around;
  align-items: center;

  .bot-remove-button {
    border: 1px solid var(--body-fg-color);
    border-radius: 2px;
    background: transparent;
    color: var(--body-fg-color);
    transition: background 0.2s ease-in-out;
    cursor: pointer;

    &:hover {
      background: var(--mask-bg-1);
    }
  }
}

@media screen and (max-width: 300px) {
  .account-info-table {
    grid-template-columns: 100%;
    grid-row-gap: 10px;
  }
}
</style>

<script charset="utf-8">
export default {
  name: 'Account',
  data() {
    return {
      user: {
        username: null,
        id: null
      },
      botfile: null,
      isDraggedOver: false
    };
  },
  mounted() {
    this.$store.dispatch('login/grabToken');
    this.user = {
      username: this.$store.state.login.username,
      id: this.$store.state.login.id
    };
  },
  methods: {
    dropHandler(ev) {
      this.isDraggedOver = false;
      if (ev.dataTransfer.items) {
        for (let i = 0; i < ev.dataTransfer.items.length; i++) {
          if (ev.dataTransfer.items[i].kind === 'file') {
            const file = ev.dataTransfer.items[i].getAsFile();
            if (this.validateBotfile(file)) {
              this.botfile = {
                name: this.getBotname(file),
                file
              };
            }
          }
        }
      }
    },
    fileInputHandler(ev) {
      const fileInput = document.querySelector('#input-bot');

      if (fileInput.files.length > 0) {
        const file = fileInput.files[0];
        if (this.validateBotfile(file)) {
          this.botfile = {
            name: this.getBotname(file),
            file
          };
        }
      }
    },
    submitBot(ev) {
      let uploadUrl = process.env.apiurl + '/user/upload';
      this.$axios.$post(uploadUrl, this.botfile);
    },
    /**
     * Validate if file has the format we want
     * @param   {object}    file    File object
     * @return  {boolean}
     */
    validateBotfile(file) {
      if (file.name.includes('.tar.gz')) {
        return true;
      }
    },
    /**
     * Returns bot name of file
     * @param   {object}    file    File object
     * @return  {String}
     */
    getBotname(file) {
      return file.name.replace('.tar.gz', '');
    }
  }
};
</script>
