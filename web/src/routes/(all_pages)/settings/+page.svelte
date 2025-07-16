<script lang="ts">
	import Icon from '@iconify/svelte';
	import cloneDeep from 'lodash/cloneDeep';

	import FancyHeader from '$lib/components/FancyHeader.svelte';
	import Button from '$lib/components/Button.svelte';
	import { type OnlineState } from '$lib/components/types/OnlineStatus';

	import { updateSettings, settingsStore } from '$lib/stores/settings';
	import { getHealth } from '$lib/api/api';
	import OnlineStatus from '$lib/components/OnlineStatus.svelte';

	const settingsClone = $state(cloneDeep($settingsStore));

	let apiUrlChoice = $state(
		settingsClone.apiBaseurl === 'https://api.salz.life'
			? 'https://api.salz.life'
			: settingsClone.apiBaseurl.startsWith('http://localhost')
				? 'http://localhost'
				: '',
	);
	let localhostPort = $state(
		settingsClone.apiBaseurl.startsWith('http://localhost')
			? settingsClone.apiBaseurl.substring(settingsClone.apiBaseurl.lastIndexOf(':') + 1)
			: 8080,
	);
	let otherApiUrl = $state((() => (apiUrlChoice === '' ? settingsClone.apiBaseurl : ''))());
	let isApiOnline: OnlineState = $state('offline');
	let updateApiOnlineStatus = $derived(() => {
		if (isApiOnline !== 'online') isApiOnline = 'unknown';
		if (settingsClone.apiBaseurl !== '') {
			try {
				// We'll do this to verify that the given url is valid, before we start
				// peppering it with API calls
				new URL(settingsClone.apiBaseurl);
			} catch {
				return;
			}
			getHealth(settingsClone.apiBaseurl).then((resp) => {
				isApiOnline = resp.isOk() ? 'online' : 'offline';
			});
		} else {
			isApiOnline = 'unknown';
		}
	});

	const errorMessages = $state<Record<'otherApiUrl', string>>({ otherApiUrl: '' });
	let hasErrors = $derived(Object.values(errorMessages).some((v) => v !== ''));
	let hasSaved = $state(false);

	$effect(() => {
		switch (apiUrlChoice) {
			case 'https://api.salz.life':
				settingsClone.apiBaseurl = 'https://api.salz.life';
				errorMessages.otherApiUrl = '';
				break;
			case 'http://localhost':
				settingsClone.apiBaseurl = `http://localhost:${localhostPort}`;
				errorMessages.otherApiUrl = '';
				break;
			case '':
				settingsClone.apiBaseurl = otherApiUrl;
				if (otherApiUrl === '') errorMessages.otherApiUrl = 'Must not be empty';
				break;
		}
		isApiOnline = 'unknown';
		hasSaved = false;
	});

	function saveSettings() {
		updateSettings(settingsClone);
		hasSaved = true;
	}
</script>

<main>
	<FancyHeader headerLevel="1">
		{#snippet icon()}
			<Icon icon="tabler:settings" />
		{/snippet}
		Settings
	</FancyHeader>

	<form>
		<fieldset class="api-url">
			<legend>API URL</legend>

			<div>
				<input
					type="radio"
					id="salz-life"
					name="apiUrl"
					bind:group={apiUrlChoice}
					value="https://api.salz.life"
					checked={apiUrlChoice === 'https://api.salz.life'}
				/>
				<label for="salz-life">https://api.salz.life</label>
			</div>

			<div>
				<input
					type="radio"
					id="localhost"
					name="apiUrl"
					bind:group={apiUrlChoice}
					value="http://localhost"
					checked={apiUrlChoice === 'http://localhost'}
				/>
				<label for="localhost">http://localhost:</label>
				<input
					type="number"
					id="localhost-port"
					name="apiUrl-locdalhost-port"
					bind:value={localhostPort}
					disabled={apiUrlChoice !== 'http://localhost'}
				/>
			</div>

			<div>
				<input
					type="radio"
					id="other"
					name="apiUrl"
					bind:group={apiUrlChoice}
					value=""
					checked={apiUrlChoice === ''}
				/>
				<input
					type="text"
					id="custom-api-url"
					name="custom-api-url"
					bind:value={otherApiUrl}
					placeholder="Other (please specify)"
					disabled={apiUrlChoice !== ''}
				/>
				{#if apiUrlChoice === ''}
					<span role="alert" class="error-msg">
						{errorMessages.otherApiUrl}
					</span>
				{/if}
			</div>
		</fieldset>

		<div class="api-status">
			API Status: <Button onClick={updateApiOnlineStatus}><Icon icon="tabler:reload" /></Button>
			<OnlineStatus onlineState={isApiOnline} />
		</div>

		<fieldset class="form-controls">
			<Button onClick={saveSettings} isDisabled={hasErrors}>Save</Button>
			{#if hasErrors}
				<span role="alert" class="error-msg">There are errors in the form.</span>
			{/if}
			{#if hasSaved}
				<span role="alert" class="success-msg">Saved!</span>
			{/if}
		</fieldset>
	</form>
</main>

<style>
	main {
		border: 0.1rem solid gray;
		padding: 0.3em 1.5em 1em 0em;
		border-radius: 5px 5px 10px 10px;
		min-height: 30vh;
		width: 60vw;
		margin: 2em auto 5em auto;
		background: var(--colour-background);
	}

	main > form {
		margin-top: 2em;
		padding-left: 1.5em;
	}

	fieldset {
		padding: 0 1rem 1rem 1rem;
	}

	form > * + * {
		margin-top: 1.5rem;
	}

	fieldset.api-url > div {
		display: flex;
		align-items: center;
	}

	fieldset.api-url > div > * + :global(*) {
		margin-left: 0.5em;
	}

	.api-status {
		display: flex;
		align-items: center;
	}

	.api-status :global(button) {
		height: 1em;
		padding: 0em 1em;
	}

	.form-controls {
		border: 0;
	}

	.error-msg {
		color: red;
	}

	.success-msg {
		color: green;
	}
</style>
