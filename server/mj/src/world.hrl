%% return string()
-define(WORLD_VERSION, element(2, application:get_key(world, vsn))).
-define(WORLD_DESCRIPTION, element(2, application:get_key(world, description))).