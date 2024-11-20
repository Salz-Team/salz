<script lang="ts">
	import type { MatchResult as MatchResultModel } from '$lib/models';
	import { Temporal } from 'temporal-polyfill';
	import MatchResult from './MatchResult.svelte';

	interface Props {
		matchResults: MatchResultModel[];
	}

	let { matchResults }: Props = $props();

	let localTimezoneId = $derived(Temporal.Now.timeZoneId());
</script>

{#each matchResults as matchResult}
	<MatchResult
		{matchResult}
		datetimeDisplay={matchResult.timestamp
			.toZonedDateTimeISO(localTimezoneId)
			.toPlainDateTime()
			.toLocaleString()}
	/>
{/each}
