# Protected temporal-model lifecycle

The temporal models are expensive persistent artifacts, not ordinary ephemeral
targets. The normal pipeline audits and reuses them. Fitting requires both a
current lifecycle need and a unique, definition-bound request in
`general_model_run_requests.csv`.

An empty request ledger authorizes zero fits. A request is consumed as soon as
its `fit_started` event is written, so failures and interruptions require a new
request ID. Evaluation and prediction may continue automatically after an
authorized fit succeeds.
