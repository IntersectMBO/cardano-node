--
-- tables containing data
--

-- describes a cluster run
CREATE TABLE cluster_run
    ( id SERIAL PRIMARY KEY
    , run_profile TEXT NOT NULL
    , run_commit TEXT NOT NULL
    , run_at TIMESTAMPTZ NOT NULL
    , run_published BOOLEAN NOT NULL DEFAULT false

    , CONSTRAINT un_run_profile UNIQUE (run_profile, run_commit, run_at)
);

CREATE TABLE run_info
    ( meta JSONB NOT NULL
    , run_id INTEGER NOT NULL

    , CONSTRAINT un_info_run_id UNIQUE (run_id)
    , CONSTRAINT fk_info_run_id FOREIGN KEY (run_id) REFERENCES cluster_run (id)
        ON DELETE CASCADE
);

CREATE TABLE run_result
    ( blockprop JSONB
    , clusterperf JSONB
    , run_id INTEGER NOT NULL

    , CONSTRAINT un_result_run_id UNIQUE (run_id)
    , CONSTRAINT fk_result_run_id FOREIGN KEY (run_id) REFERENCES cluster_run (id)
        ON DELETE CASCADE
);
