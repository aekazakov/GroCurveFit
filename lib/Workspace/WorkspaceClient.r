library(jsonlite)
library(httr)

WorkspaceClient <- function(url, token = NULL, flatten = TRUE, 
        simplifyDataFrame = FALSE, simplifyVector = FALSE, simplifyMatrix = FALSE) {
    ret_client_object_ref <- list(url = url, token = token)

    ret_client_object_ref[['json_rpc_call']] <- function(method_name, param_list) {
        body_text = toJSON(
            list(
                id=1,version=unbox("1.1"),
                method=unbox(method_name),
                params=param_list
            )
        )
        resp <- POST(ret_client_object_ref[['url']],
            accept_json(),
            add_headers("Content-Type" = "application/json",
                "Authorization" = unbox(ret_client_object_ref[['token']])),
            body = body_text
        )
        parsed <- fromJSON(content(resp, "text"), 
            flatten=ret_client_object_ref[['flatten']], 
            simplifyDataFrame=ret_client_object_ref[['simplifyDataFrame']], 
            simplifyVector=ret_client_object_ref[['simplifyVector']], 
            simplifyMatrix=ret_client_object_ref[['simplifyMatrix']])
        if (resp[['status_code']] != 200) {
            error <- parsed[['error']]
            if (is.null(error)) {
                stop_for_status(resp)
            }
            message <- error[['error']]
            if (is.null(message)) {
                message <- error[['data']]
            }
            if (is.null(message)) {
                message <- error[['message']]
            }
            stop(message)
        }
        return(parsed[['result']])
    }

    ret_client_object_ref[['ver']] <- function() {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.ver", list(
                
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['create_workspace']] <- function(params) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.create_workspace", list(
                params
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['alter_workspace_metadata']] <- function(params) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.alter_workspace_metadata", list(
                params
        ))
        return(ret)
    }

    ret_client_object_ref[['clone_workspace']] <- function(params) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.clone_workspace", list(
                params
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['lock_workspace']] <- function(wsi) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.lock_workspace", list(
                wsi
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['get_workspacemeta']] <- function(params) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.get_workspacemeta", list(
                params
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['get_workspace_info']] <- function(wsi) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.get_workspace_info", list(
                wsi
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['get_workspace_description']] <- function(wsi) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.get_workspace_description", list(
                wsi
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['set_permissions']] <- function(params) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.set_permissions", list(
                params
        ))
        return(ret)
    }

    ret_client_object_ref[['set_global_permission']] <- function(params) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.set_global_permission", list(
                params
        ))
        return(ret)
    }

    ret_client_object_ref[['set_workspace_description']] <- function(params) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.set_workspace_description", list(
                params
        ))
        return(ret)
    }

    ret_client_object_ref[['get_permissions']] <- function(wsi) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.get_permissions", list(
                wsi
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['save_object']] <- function(params) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.save_object", list(
                params
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['save_objects']] <- function(params) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.save_objects", list(
                params
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['get_object']] <- function(params) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.get_object", list(
                params
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['get_object_provenance']] <- function(object_ids) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.get_object_provenance", list(
                object_ids
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['get_objects']] <- function(object_ids) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.get_objects", list(
                object_ids
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['get_object_subset']] <- function(sub_object_ids) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.get_object_subset", list(
                sub_object_ids
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['get_object_history']] <- function(object) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.get_object_history", list(
                object
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['list_referencing_objects']] <- function(object_ids) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.list_referencing_objects", list(
                object_ids
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['list_referencing_object_counts']] <- function(object_ids) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.list_referencing_object_counts", list(
                object_ids
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['get_referenced_objects']] <- function(ref_chains) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.get_referenced_objects", list(
                ref_chains
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['list_workspaces']] <- function(params) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.list_workspaces", list(
                params
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['list_workspace_info']] <- function(params) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.list_workspace_info", list(
                params
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['list_workspace_objects']] <- function(params) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.list_workspace_objects", list(
                params
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['list_objects']] <- function(params) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.list_objects", list(
                params
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['get_objectmeta']] <- function(params) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.get_objectmeta", list(
                params
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['get_object_info']] <- function(object_ids, includeMetadata) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.get_object_info", list(
                object_ids, includeMetadata
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['get_object_info_new']] <- function(params) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.get_object_info_new", list(
                params
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['rename_workspace']] <- function(params) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.rename_workspace", list(
                params
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['rename_object']] <- function(params) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.rename_object", list(
                params
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['copy_object']] <- function(params) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.copy_object", list(
                params
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['revert_object']] <- function(object) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.revert_object", list(
                object
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['hide_objects']] <- function(object_ids) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.hide_objects", list(
                object_ids
        ))
        return(ret)
    }

    ret_client_object_ref[['unhide_objects']] <- function(object_ids) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.unhide_objects", list(
                object_ids
        ))
        return(ret)
    }

    ret_client_object_ref[['delete_objects']] <- function(object_ids) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.delete_objects", list(
                object_ids
        ))
        return(ret)
    }

    ret_client_object_ref[['undelete_objects']] <- function(object_ids) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.undelete_objects", list(
                object_ids
        ))
        return(ret)
    }

    ret_client_object_ref[['delete_workspace']] <- function(wsi) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.delete_workspace", list(
                wsi
        ))
        return(ret)
    }

    ret_client_object_ref[['undelete_workspace']] <- function(wsi) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.undelete_workspace", list(
                wsi
        ))
        return(ret)
    }

    ret_client_object_ref[['request_module_ownership']] <- function(mod) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.request_module_ownership", list(
                mod
        ))
        return(ret)
    }

    ret_client_object_ref[['register_typespec']] <- function(params) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.register_typespec", list(
                params
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['register_typespec_copy']] <- function(params) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.register_typespec_copy", list(
                params
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['release_module']] <- function(mod) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.release_module", list(
                mod
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['list_modules']] <- function(params) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.list_modules", list(
                params
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['list_module_versions']] <- function(params) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.list_module_versions", list(
                params
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['get_module_info']] <- function(params) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.get_module_info", list(
                params
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['get_jsonschema']] <- function(type) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.get_jsonschema", list(
                type
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['translate_from_MD5_types']] <- function(md5_types) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.translate_from_MD5_types", list(
                md5_types
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['translate_to_MD5_types']] <- function(sem_types) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.translate_to_MD5_types", list(
                sem_types
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['get_type_info']] <- function(type) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.get_type_info", list(
                type
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['get_all_type_info']] <- function(mod) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.get_all_type_info", list(
                mod
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['get_func_info']] <- function(func) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.get_func_info", list(
                func
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['get_all_func_info']] <- function(mod) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.get_all_func_info", list(
                mod
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['grant_module_ownership']] <- function(params) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.grant_module_ownership", list(
                params
        ))
        return(ret)
    }

    ret_client_object_ref[['remove_module_ownership']] <- function(params) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.remove_module_ownership", list(
                params
        ))
        return(ret)
    }

    ret_client_object_ref[['list_all_types']] <- function(params) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.list_all_types", list(
                params
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['administer']] <- function(command) {
        ret <- ret_client_object_ref[['json_rpc_call']]("Workspace.administer", list(
                command
        ))
        return(ret[[1]])
    }

    ret_client_object_ref[['flatten']] <- flatten
    ret_client_object_ref[['simplifyDataFrame']] <- simplifyDataFrame 
    ret_client_object_ref[['simplifyVector']] <- simplifyVector
    ret_client_object_ref[['simplifyMatrix']] <- simplifyMatrix
    class(ret_client_object_ref) <- 'WorkspaceClient'
    return(ret_client_object_ref)
}
