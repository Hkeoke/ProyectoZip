<?php
/**
 * Configuración básica para phpLDAPadmin
 */

/* Crear el almacén de datos */
$servers = new Datastore();

/* Define el servidor LDAP por defecto */
$servers->newServer('ldap_pla');
$servers->setValue('server','name','Mi Servidor LDAP');
$servers->setValue('server','host','openldap');
$servers->setValue('server','port',389);
$servers->setValue('server','base',array('dc=localhost,dc=com'));
$servers->setValue('server','tls',false);

/* Configuración de autenticación */
$servers->setValue('login','auth_type','session');
$servers->setValue('login','bind_id','cn=admin,dc=localhost,dc=com');
$servers->setValue('login','bind_pass','admin');
$servers->setValue('login','attr','dn');
$servers->setValue('login','fallback_dn',false);
$servers->setValue('login','class',array());

/* Control de errores */
error_reporting(E_ALL & ~E_NOTICE & ~E_DEPRECATED);
ini_set('display_errors', 'Off');
?> 