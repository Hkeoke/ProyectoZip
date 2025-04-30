#!/bin/sh

# Configurar OpenLDAP
if [ ! -f /etc/openldap/slapd.d/cn=config.ldif ]; then
    # Crear configuración inicial
    mkdir -p /etc/openldap/slapd.d
    mkdir -p /var/lib/openldap
    # Crear directorio para pid y otros archivos
    mkdir -p /var/run/openldap
    chown ldap:ldap /var/run/openldap

    # Configurar slapd.conf
    cat > /etc/openldap/slapd.conf << EOF
include /etc/openldap/schema/core.schema
include /etc/openldap/schema/cosine.schema
include /etc/openldap/schema/inetorgperson.schema
include /etc/openldap/schema/nis.schema

pidfile /var/run/openldap/slapd.pid
argsfile /var/run/openldap/slapd.args

modulepath /usr/lib/openldap
moduleload back_mdb

database mdb
suffix "dc=${LDAP_DOMAIN},dc=com"
rootdn "cn=admin,dc=${LDAP_DOMAIN},dc=com"
rootpw ${LDAP_ADMIN_PASSWORD}
directory /var/lib/openldap

index objectClass eq
EOF

    # Inicializar la base de datos
    slapadd -F /etc/openldap/slapd.d -n 0 -l /etc/openldap/slapd.conf
    chown -R ldap:ldap /etc/openldap/slapd.d
    chown -R ldap:ldap /var/lib/openldap
fi

# Asegurar que el directorio para pid existe siempre
mkdir -p /var/run/openldap
chown ldap:ldap /var/run/openldap

# Iniciar slapd
exec "$@" 