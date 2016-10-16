%% return string()
-define(QP_VERSION, element(2, application:get_key(qp, vsn))).
-define(QP_DESCRIPTION, element(2, application:get_key(qp, description))).