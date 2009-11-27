#!/bin/bash

exec erl -boot start_sasl -s crypto -pa ebin -s membox -membox_config $1